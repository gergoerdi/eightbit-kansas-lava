{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EightBit.PET.VIA.Interruptor where

import MOS6502.Types
import MOS6502.Utils

import Language.KansasLava

import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Matrix (Matrix)
import qualified Data.Sized.Matrix as Matrix
import Data.Bits
import Prelude hiding (sequence, sequence_)
import Data.Foldable (sequence_)

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
