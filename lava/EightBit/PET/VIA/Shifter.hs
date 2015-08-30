{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module EightBit.PET.VIA.Shifter where

import MOS6502.Types
import MOS6502.Utils

import Language.KansasLava

import Data.Sized.Ix
import Data.Sized.Unsigned

data ShiftMode = SRFree
               | SRTimer2
               | SRClock
               | SRExternal
               deriving (Eq, Ord, Show, Enum, Bounded)
$(repBitRep ''ShiftMode 2); instance BitRep ShiftMode where bitRep = bitRepEnum

shifter :: forall clk s. (Clock clk)
        => Signal clk U3
        -> Signal clk Bool
        -> Signal clk Bool
        -> Signal clk Bool
        -> Signal clk (Enabled (Unsigned X0))
        -> Signal clk (Enabled U8)
        -> RTL s clk (Reg s clk Bool, Signal clk Bool, Signal clk Bool, Signal clk Byte)
shifter acr432 input clkTimer clkExternal a w = do
    sr <- newReg 0
    counter <- newReg (0 :: W U8)
    int <- newReg False
    let triggered = var counter .==. 0 .&&. reg counter ./=. 0

    let clk = switchS mode [ (SRFree,     clkTimer)
                           , (SRTimer2,   clkTimer)
                           , (SRClock,    iterateS bitNot True)
                           , (SRExternal, clkExternal)
                           ]

        -- held high unless shiftIn is active
        clkIn = out .||. bitNot (mode .==. pureS SRFree) .||. clk

        -- held high unless shiftOut is active
        clkOut = bitNot out .||. clk

    WHEN (bitNot out) $ shiftIn input sr clkIn
    output <- shiftOut sr clkOut

    WHEN cs $ do
        -- a read or a write both restart the counter
        counter := 0
        CASE [ match w $ \v -> sr := v ]
    WHEN (risingEdge clk .&&. mode ./=. pureS SRFree) $ do
        counter := mux (reg counter .==. 0) (reg counter - 1, pureS maxBound)
    WHEN triggered $ int := high

    return (int, output, clk, var sr)
  where
    (cs, _) = unpackEnabled a
    (mode, out) = unappendS acr432 :: (Signal clk ShiftMode, Signal clk Bool)

shiftIn :: forall clk s. (Clock clk)
        => Signal clk Bool
        -> Reg s clk U8
        -> Signal clk Bool
        -> RTL s clk ()
shiftIn sig sr tick = do
    let (_, srHi) = unappendS (reg sr) :: (Signal clk Bool, Signal clk U7)
        (srLo, _) = unappendS (reg sr) :: (Signal clk U7, Signal clk Bool)

    let load = do
            sr := appendS sig srHi
        shift = do
            sr := appendS low srLo

    CASE [ IF (risingEdge tick) load
         , IF (fallingEdge tick) shift
         ]

shiftTest :: forall a clk s. (Clock clk)
          => Signal clk (Enabled (Enabled U8))
          -> Signal clk Bool
          -> (Reg s clk U8 -> Signal clk Bool -> RTL s clk a)
          -> RTL s clk (Signal clk Bool, Signal clk U8, a)
shiftTest cmd tick mkShifter = do
    sr <- newReg 0
    counter <- newReg (0 :: W U8)
    let reset = do
            counter := 0
        count = do
            counter := mux (reg counter .==. 0) (reg counter - 1, pureS maxBound)
        triggered = var counter .==. 0 .&&. reg counter ./=. 0

    CASE [ match cmd $ \w -> do
                reset
                CASE [ match w $ \v -> sr := v ]
         , IF (risingEdge tick) count
         ]

    res <- mkShifter sr tick
    return (triggered, var sr, res)

shiftInTest :: Seq (Bool, U8)
shiftInTest = runRTL $ do
    (finished, read, ()) <- shiftTest disabledS tick $ shiftIn sig
    return $ pack (finished, read)
  where
    tick = iterateS bitNot True
    input = [True, True, True, False, False, False, False, True] ++ repeat False
    sig = toS . concatMap (replicate 2) $ False : input

shiftOut :: forall clk s. (Clock clk)
         => Reg s clk U8
         -> Signal clk Bool
         -> RTL s clk (Signal clk Bool)
shiftOut sr tick = do
    let (srLo, rot) = unappendS (reg sr) :: (Signal clk U7, Signal clk Bool)

    let shift = do
            sr := appendS rot srLo

    WHEN (fallingEdge tick) shift

    let out = var sr `testABit` 0
    return out

shiftOutTest :: Seq (Bool, Bool, Bool)
shiftOutTest = runRTL $ do
    (finished, _, written) <- shiftTest cmds tick shiftOut
    return $ pack (fallingEdge tick, finished, written)
  where
    tick = toS $ cycle [True, True, False]
    cmds = toS $ (Just $ Just 123) : repeat Nothing
