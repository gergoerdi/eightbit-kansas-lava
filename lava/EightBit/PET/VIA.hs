{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EightBit.PET.VIA where

import MOS6502.Types
import MOS6502.Utils

import Language.KansasLava

import Data.Sized.Unsigned
import Data.Bits

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
via VIAIn{..} = undefined

timer :: forall clk. (Clock clk)
      => Signal clk Bool
      -> Signal clk Bool
      -> Signal clk (Enabled U2)
      -> Signal clk (Enabled Byte)
      -> (Signal clk Bool, Signal clk Byte)
timer countee freeRun a w = runRTL $ do
    latchLo <- newReg 0
    latchHi <- newReg 0
    let latch = appendS (var latchLo) (var latchHi)

    counter <- newReg (0 :: U16)
    let (counterLo, counterHi) = unappendS (reg counter)
    int <- newReg True
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

    return (triggered, read)
  where
    (cs, addr) = unpackEnabled a
    (we, written) = unpackEnabled w

timer1 :: forall clk. (Clock clk)
       => Signal clk Bool
       -> Signal clk (Enabled U2)
       -> Signal clk (Enabled Byte)
       -> (Signal clk Bool, Signal clk Byte)
timer1 = timer high

timer1Test :: Seq Bool
timer1Test = fst $ timer1 high a w
  where
    pipe = [Just (0x0, 0x0A), Just (0x1, 0x00)] ++ replicate 30 Nothing
    a = toS $ map (fmap fst) pipe
    w = toS $ map (fmap snd) pipe

timer2 :: forall clk. (Clock clk)
       => Signal clk Bool
       -> Signal clk (Enabled U1)
       -> Signal clk (Enabled Byte)
       -> (Signal clk Bool, Signal clk Byte)
timer2 countee a = timer countee low (mapEnabled unsigned a)

timer2Test :: Seq (Bool, Bool)
timer2Test = pack (fst $ timer2 countee a w, countee)
  where
    pipe = [Just (0x0, 3), Just (0x1, 0)] ++ replicate 30 Nothing
    a = toS $ map (fmap fst) pipe
    w = toS $ map (fmap snd) pipe

    countee = toS $ replicate 3 True ++ replicate 4 False ++ cycle (True : replicate 3 False)
