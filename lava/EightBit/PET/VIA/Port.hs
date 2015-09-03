{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module EightBit.PET.VIA.Port where

import MOS6502.Types
import MOS6502.Utils
import EightBit.PET.Utils

import Language.KansasLava
import Data.Sized.Unsigned
import Data.Sized.Ix
import Data.Sized.Matrix as Matrix

data PortAddr = DDR
              | PerifReset
              | Perif
              deriving (Eq, Ord, Show, Enum, Bounded)
$(repBitRep ''PortAddr 2); instance BitRep PortAddr where bitRep = bitRepEnum

port :: forall clk s. (Clock clk)
     => Signal clk Bool
     -> Signal clk Bool
     -> Signal clk U3
     -> Signal clk (Bool, Bool, Byte)
     -> Signal clk (Enabled PortAddr)
     -> Signal clk (Enabled Byte)
     -> RTL s clk ((Reg s clk Bool, Signal clk Bool),
                   (Reg s clk Bool, Signal clk Bool),
                   Matrix X8 (Signal clk (Enabled Bool)),
                   Signal clk Byte)
port latch ctrl1Rise _ctrl2Mode input a w = do
    ddr <- newReg 0
    inLatch <- newReg 0
    outLatch <- newReg 0
    ctrl1Int <- newReg False
    ctrl2Int <- newReg False
    WHEN we $ CASE [ IF isDDR $ ddr := written
                   , IF isPerif $ outLatch := written
                   ]
    WHEN isPerifReset $ do
        ctrl1Int := low
        ctrl2Int := low
    let trigger1 = mux ctrl1Rise (fallingEdge inC1, risingEdge inC1)
    WHEN trigger1 $ ctrl1Int := high
    -- TODO: transfer inPort to inLatch on trigger1
    let perifRead = mux latch (inPort, reg inLatch)

    let read = muxN [ (isDDR,   reg ddr)
                    , (isPerif, perifRead)
                    ]
        outPort = Matrix.zipWith packEnabled (unbus $ reg ddr) (unbus $ reg outLatch)

    return ((ctrl1Int, trigger1), (ctrl2Int, undefined), outPort, read)
  where
    (inC1, _inC2, inPort) = unpack input

    (we, written) = unpackEnabled w

    (cs, addr) = unpackEnabled a
    isDDR = cs .&&. addr .==. pureS DDR
    isPerifReset = cs .&&. addr .==. pureS PerifReset
    isPerif = isPerifReset .||. (cs .&&. addr .==. pureS Perif)
