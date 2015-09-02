{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module EightBit.PET.VIA where

import EightBit.PET.VIA.Timer
import EightBit.PET.VIA.Shifter
import EightBit.PET.VIA.Interruptor

import MOS6502.Types
import MOS6502.Utils
import EightBit.PET.Utils
import Language.Literals.Binary

import Language.KansasLava

import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Matrix (Matrix)
import qualified Data.Sized.Matrix as Matrix
import Prelude hiding (foldr1)
import Data.Foldable (foldr1)

data VIAIn clk = VIAIn{ viaA :: Signal clk (Enabled U4)
                      , viaW :: Signal clk (Enabled Byte)
                      , viaInputA, viaInputB :: Signal clk (Bool, Bool, Byte)
                      }
               deriving Show

data VIAOut clk = VIAOut{ viaR :: Signal clk Byte
                        , viaIRQ :: Signal clk ActiveLow
                        , viaOutputA :: Signal clk (Bool, Byte)
                        , viaOutputB :: Signal clk (Bool, Bool, Byte)
                        }
                deriving Show

via :: forall clk. (Clock clk) => VIAIn clk -> VIAOut clk
via VIAIn{..} = runRTL $ do
    pcr <- newReg 0
    WHEN (cs .&&. isPCR .&&. we) $
      pcr := written

    acr <- newReg 0
    WHEN (cs .&&. isACR .&&. we) $
      acr := written

    let [_acr0, _acr1, acr2, acr3, acr4, _acr5, _acr6, _acr7] =
            Matrix.toList $ unbus $ reg acr

    let acr432 = bus $ Matrix.fromList [acr2, acr3, acr4]

    (timer1Int, _trigger1, _, timer1R) <- component isTimer1 $ timer1 low
    (timer2Int, _trigger2, timerLo2, timer2R) <- component isTimer2 $ timer2 high
    (shiftInt, _shiftOut, _shiftClk, shiftR) <- component isShifter $ shifter acr432 cb2In cb1In timerLo2

    let ints = Matrix.fromList
               [ undefined
               , undefined
               , shiftInt
               , undefined
               , undefined
               , timer2Int
               , timer1Int
               ]
    (irq, intR) <- component isInterruptor $ interruptor ints

    let viaR = muxN [ (isTimer1,      timer1R)
                    , (isTimer2,      timer2R)
                    , (isInterruptor, intR)
                    , (isShifter,     shiftR)
                    , (isACR,         reg acr)
                    , (isPCR,         reg pcr)
                    ]
        viaIRQ = bitNot irq
        viaOutputA = undefined

        cb1Out = undefined
        cb2Out = undefined
        pbOut = undefined
        viaOutputB = pack (cb1Out, cb2Out, pbOut)
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
    (cb1In, cb2In, _pbIn) = unpack viaInputB

    [_rs0, rs1, rs2, rs3] = Matrix.toList $ unbus addr

    isTimer1 = foldr1 (.&&.) $ zipWith (.==.) [rs3, rs2] [low, high]
    isTimer2 = foldr1 (.&&.) $ zipWith (.==.) [rs3, rs2, rs1] [high, low, low]

    isShifter = addr .==. [b|1010|]

    isIER = addr .==. [b|1110|]
    isIFR = addr .==. [b|1101|]
    isInterruptor = isIER .||. isIFR

    isACR = addr .==. [b|1011|]
    isPCR = addr .==. [b|1100|]
