{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module EightBit.PET.VIA where

import EightBit.PET.VIA.Timer
import EightBit.PET.VIA.Shifter
import EightBit.PET.VIA.Interruptor
import EightBit.PET.VIA.Port

import MOS6502.Types
import MOS6502.Utils
import EightBit.PET.Utils
import Language.Literals.Binary

import Language.KansasLava

import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Matrix (Matrix, (!))
import qualified Data.Sized.Matrix as Matrix

data VIAIn clk = VIAIn{ viaA :: Signal clk (Enabled U4)
                      , viaW :: Signal clk (Enabled Byte)
                      , viaInputA, viaInputB :: Signal clk (Bool, Bool, Byte)
                      }
               deriving Show

data VIAOut clk = VIAOut{ viaR :: Signal clk Byte
                        , viaIRQ :: Signal clk ActiveLow
                        , viaOutputA :: (Signal clk (Enabled Bool), Matrix X8 (Signal clk (Enabled Bool)))
                        , viaOutputB :: (Signal clk (Enabled Bool), Signal clk (Enabled Bool), Matrix X8 (Signal clk (Enabled Bool)))
                        }
                deriving Show

via :: forall clk. (Clock clk) => VIAIn clk -> VIAOut clk
via VIAIn{..} = runRTL $ do
    pcr <- newReg 0
    WHEN (cs .&&. isPCR .&&. we) $
      pcr := written
    let [ca1c, pcr1, pcr2, pcr3, cb1c, pcr5, pcr6, pcr7] =
            Matrix.toList . unbus $ reg pcr
        ca2c = bitwise $ bus . Matrix.fromList $ [pcr1, pcr2, pcr3]
        cb2c = bitwise $ bus . Matrix.fromList $ [pcr5, pcr6, pcr7]

    acr <- newReg 0
    WHEN (cs .&&. isACR .&&. we) $
      acr := written

    let [latchA, latchB, acr2, acr3, shiftOut, t2FromPB6, t1Free, t1Out] =
            Matrix.toList . unbus $ reg acr
        shiftMode = bitwise $ bus . Matrix.fromList $ [acr2, acr3]
        -- _timer2Mode = acr5
        t2Source = let pb6 = pbIn `testABit` 6
                   in mux t2FromPB6 (high, pb6)

    (timer1Int, trigger1, _, timer1R) <- component isTimer1 $ timer1 t1Free
    (timer2Int, _trigger2, timerLo2, timer2R) <- component isTimer2 $ timer2 t2Source
    (shiftInt, shiftOut, shiftClk, shiftR) <- component isShifter $ shifter shiftOut shiftMode cb2In cb1In timerLo2

    let isPortA = isDDRA .||. isORA .||. isORA'
    let portAAddr = packEnabled (cs .&&. isPortA) $
                    muxN [ (isDDRA, pureS DDR)
                         , (isORA,  pureS PerifReset)
                         , (isORA', pureS Perif)
                         ]
    ((ca1Int, _ca1Trigger), (ca2Int, _ca2Trigger), paOut, paR) <-
        port latchA ca1c ca2c viaInputA portAAddr viaW

    let isPortB = isDDRB .||. isORB
    let portBAddr = packEnabled (cs .&&. isPortB) $
                    muxN [ (isDDRB, pureS DDR)
                         , (isORB, pureS PerifReset)
                         ]
    ((cb1Int, _cb1Trigger), (cb2Int, _cb2Trigger), pbOut, pbR) <-
        port latchB cb1c cb2c viaInputB portBAddr viaW

    let pb7' = mux t1Out (pbOut ! 7, enabledS trigger1)
        pbOut' = Matrix.fromList $ init (Matrix.toList pbOut) ++ [pb7']

    let ints = Matrix.fromList
               [ ca2Int
               , ca1Int
               , shiftInt
               , cb2Int
               , cb1Int
               , timer2Int
               , timer1Int
               ]
    (irq, intR) <- component isInterruptor $ interruptor ints

    let viaR = muxN [ (isPortA,       paR)
                    , (isPortB,       pbR)
                    , (isTimer1,      timer1R)
                    , (isTimer2,      timer2R)
                    , (isInterruptor, intR)
                    , (isShifter,     shiftR)
                    , (isACR,         reg acr)
                    , (isPCR,         reg pcr)
                    ]
        viaIRQ = bitNot irq

        ca2Out = undefined -- TODO: handshake...
        cb1Out = shiftClk -- TODO: or handshake...
        cb2Out = shiftOut -- TODO: or handshake...
        viaOutputA = (ca2Out, paOut)
        viaOutputB = (cb1Out, cb2Out, pbOut')
    return VIAOut{..}
  where
    component :: (Rep a, Num a)
              => Signal clk Bool
              -> (Signal clk (Enabled a) -> Signal clk (Enabled U8) -> RTL s clk r)
              -> RTL s clk r
    component sel mkPart = mkPart (packEnabled (cs .&&. sel) (unsigned addr)) viaW

    (cs, addr) = unpackEnabled viaA
    (we, written) = unpackEnabled viaW

    (_ca1In, _ca2In, _paIn) = unpack viaInputA
    (cb1In, cb2In, pbIn) = unpack viaInputB

    [_rs0, rs1, rs2, rs3] = Matrix.toList $ unbus addr

    isORA  = addr .==. [b|0001|]
    isORA' = addr .==. [b|1111|]
    isDDRA = addr .==. [b|0011|]

    isORB  = addr .==. [b|0000|]
    isDDRB = addr .==. [b|0010|]

    isTimer1 = bus (Matrix.fromList [rs2, rs3]) .==. pureS ([b|01|] :: U2)
    isTimer2 = bus (Matrix.fromList [rs1, rs2, rs3]) .==. pureS ([b|100|] :: U3)

    isShifter = addr .==. [b|1010|]

    isIER = addr .==. [b|1110|]
    isIFR = addr .==. [b|1101|]
    isInterruptor = isIER .||. isIFR

    isACR = addr .==. [b|1011|]
    isPCR = addr .==. [b|1100|]
