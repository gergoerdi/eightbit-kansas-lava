{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EightBit.PET.VIA.Timer where

import MOS6502.Types
import MOS6502.Utils

import Language.KansasLava
import Data.Sized.Unsigned

timer :: forall clk s. (Clock clk)
      => Signal clk Bool
      -> Signal clk Bool
      -> Signal clk (Enabled U2)
      -> Signal clk (Enabled Byte)
      -> RTL s clk (Reg s clk Bool, Signal clk Bool, Signal clk Bool, Signal clk Byte)
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

    return (int, triggered, counterLo .==. 0, read)
  where
    (cs, addr) = unpackEnabled a
    (we, written) = unpackEnabled w

timer1 :: forall clk s. (Clock clk)
       => Signal clk Bool
       -> Signal clk (Enabled U2)
       -> Signal clk (Enabled Byte)
       -> RTL s clk (Reg s clk Bool, Signal clk Bool, Signal clk Bool, Signal clk Byte)
timer1 = timer high

timer1Test :: Seq (Bool, Bool)
timer1Test = runRTL $ do
    (int, triggered, _, _) <- timer1 high a w
    return $ pack (triggered, var int)
  where
    pipe = [Just (0x0, 0x0A), Just (0x1, 0x00)] ++ replicate 30 Nothing
    a = toS $ map (fmap fst) pipe
    w = toS $ map (fmap snd) pipe

timer2 :: forall clk s. (Clock clk)
       => Signal clk Bool
       -> Signal clk (Enabled U1)
       -> Signal clk (Enabled Byte)
       -> RTL s clk (Reg s clk Bool, Signal clk Bool, Signal clk Bool, Signal clk Byte)
timer2 countee a = timer countee low (mapEnabled unsigned a)

timer2Test :: Seq (Bool, Bool, Bool)
timer2Test = runRTL $ do
    (int, triggered, _, _) <- timer2 countee a w
    return $ pack (triggered, var int, countee)
  where
    pipe = [Just (0x0, 3), Just (0x1, 0)] ++ replicate 30 Nothing
    a = toS $ map (fmap fst) pipe
    w = toS $ map (fmap snd) pipe

    countee = toS $ replicate 3 True ++ replicate 4 False ++ cycle (True : replicate 3 False)
