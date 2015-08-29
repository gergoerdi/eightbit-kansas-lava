{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module EightBit.PET.VIA where

import MOS6502.Types
import MOS6502.Utils
import Language.Literals.Binary

import Language.KansasLava

import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Matrix (Matrix)
import qualified Data.Sized.Matrix as Matrix
import Data.Bits
import Prelude hiding (foldr1, sequence, sequence_)
import Data.Foldable (foldr1, sequence_)
import Data.Traversable (sequence)

data VIAIn clk = VIAIn{ viaA :: Signal clk (Enabled U4)
                      , viaW :: Signal clk (Enabled Byte)
                      , viaInputA, viaInputB :: Signal clk (U2, Byte)
                      }
               deriving Show

data VIAOut clk = VIAOut{ viaR :: Signal clk Byte
                        , viaIRQ :: Signal clk ActiveLow
                        , viaOutputA, viaOutputB :: Signal clk (U2, Byte)
                        }
                deriving Show

via :: forall clk. (Clock clk) => VIAIn clk -> VIAOut clk
via VIAIn{..} = runRTL $ do
    (timer1Int, _trigger1, timer1R) <- component isTimer1 $ timer1 low
    (timer2Int, _trigger2, timer2R) <- component isTimer2 $ timer2 high

    let ints = Matrix.fromList [ undefined, undefined, undefined, undefined, undefined, timer2Int, timer1Int ]
    (irq, intR) <- component isInterruptor $ interruptor ints

    let viaR = muxN [ (isTimer1, timer1R)
                    , (isTimer2, timer2R)
                    , (isInterruptor, intR)
                    ]
        viaIRQ = bitNot irq
        viaOutputA = undefined
        viaOutputB = undefined
    return VIAOut{..}
  where
    component sel mkPart = mkPart (packEnabled (cs .&&. sel) (unsigned addr)) viaW

    (cs, addr) = unpackEnabled viaA
    (_we, _written) = unpackEnabled viaW

    [_rs0, rs1, rs2, rs3] = Matrix.toList . unpackMatrix $ bitwise addr

    isTimer1 = foldr1 (.&&.) $ zipWith (.==.) [rs3, rs2] [low, high]
    isTimer2 = foldr1 (.&&.) $ zipWith (.==.) [rs3, rs2, rs1] [high, low, low]

    isIER = addr .==. [b|1110|]
    isIFR = addr .==. [b|1101|]
    isInterruptor = isIER .||. isIFR

interruptor :: forall clk s. (Clock clk)
            => Matrix X7 (Reg s clk Bool)
            -> Signal clk (Enabled U1)
            -> Signal clk (Enabled Byte)
            -> RTL s clk (Signal clk Bool, Signal clk Byte)
interruptor ints a w = do
    ier <- newReg (0 :: U7)
    let ifr0 = bitwise . packMatrix $ fmap reg ints
        irq = (ifr0 .&. reg ier) ./=. 0
        ifr = appendS ifr0 irq

    WHEN cs $
      CASE [ match w $ \w -> do
                  let (val, mode) = unappendS w
                      val' = unpackMatrix . bitwise $ val
                  switch addr $ \sel -> case sel of
                      0x0 -> ier := mux mode (reg ier .&. negate val, reg ier .|. val)
                      0x1 -> sequence_ $ Matrix.zipWith (\b -> WHEN b . (:= low)) val' ints
           ]

    let read = switchS addr [ (0x0, unsigned $ reg ier)
                            , (0x1, ifr)
                           ]
    return (irq, read)
  where
    (cs, addr) = unpackEnabled a

timer :: forall clk s. (Clock clk)
      => Signal clk Bool
      -> Signal clk Bool
      -> Signal clk (Enabled U2)
      -> Signal clk (Enabled Byte)
      -> RTL s clk (Reg s clk Bool, Signal clk Bool, Signal clk Byte)
timer countee freeRun a w = do
    latchLo <- newReg 0
    latchHi <- newReg 0
    let latch = appendS (var latchLo) (var latchHi)

    counter <- newReg (0 :: U16)
    let (counterLo, counterHi) = unappendS (reg counter)
    int <- newReg False
    let restart = int := low

    WHEN cs . CASE $
      [ IF we $ switch addr $ \sel -> case sel of
             0x0 -> latchLo := written
             0x1 -> do
                 latchHi := written
                 counter := latch
                 restart
             0x2 -> latchLo := written
             0x3 -> do
                 latchHi := written
                 restart
      , OTHERWISE $ WHEN (addr .==. 0x0) restart
      ]

    let counter' = reg counter - 1
        triggered = countee .&&. counter' .==. 0 .&&. reg counter ./=. 0
    WHEN triggered $ int := high
    WHEN countee $ counter := mux (triggered .&&. freeRun) (counter', latch)

    let read = switchS addr [ (0x0, counterLo)
                            , (0x1, counterHi)
                            , (0x2, reg latchLo)
                            , (0x3, reg latchHi)
                            ]

    return (int, triggered, read)
  where
    (cs, addr) = unpackEnabled a
    (we, written) = unpackEnabled w

timer1 :: forall clk s. (Clock clk)
       => Signal clk Bool
       -> Signal clk (Enabled U2)
       -> Signal clk (Enabled Byte)
       -> RTL s clk (Reg s clk Bool, Signal clk Bool, Signal clk Byte)
timer1 = timer high

timer1Test :: Seq (Bool, Bool)
timer1Test = runRTL $ do
    (int, triggered, _) <- timer1 high a w
    return $ pack (triggered, var int)
  where
    pipe = [Just (0x0, 0x0A), Just (0x1, 0x00)] ++ replicate 30 Nothing
    a = toS $ map (fmap fst) pipe
    w = toS $ map (fmap snd) pipe

timer2 :: forall clk s. (Clock clk)
       => Signal clk Bool
       -> Signal clk (Enabled U1)
       -> Signal clk (Enabled Byte)
       -> RTL s clk (Reg s clk Bool, Signal clk Bool, Signal clk Byte)
timer2 countee a = timer countee low (mapEnabled unsigned a)

timer2Test :: Seq (Bool, Bool, Bool)
timer2Test = runRTL $ do
    (int, triggered, _) <- timer2 countee a w
    return $ pack (triggered, var int, countee)
  where
    pipe = [Just (0x0, 3), Just (0x1, 0)] ++ replicate 30 Nothing
    a = toS $ map (fmap fst) pipe
    w = toS $ map (fmap snd) pipe

    countee = toS $ replicate 3 True ++ replicate 4 False ++ cycle (True : replicate 3 False)
