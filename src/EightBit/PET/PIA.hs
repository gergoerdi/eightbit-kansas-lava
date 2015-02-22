{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EightBit.PET.PIA where

import MOS6502.Types
import MOS6502.Utils

import Language.KansasLava

import Data.Sized.Unsigned
import Data.Bits

data PIAIn clk = PIAIn{ piaA :: Signal clk (Enabled U2)
                      , piaW :: Signal clk (Enabled Byte)
                      , piaPerifA :: (Signal clk Bool, Signal clk Bool)
                      , piaPerifB :: (Signal clk Bool, Signal clk Bool)
                      }
               deriving Show

data PIAOut clk = PIAOut{ piaR :: Signal clk Byte
                        , piaIRQA :: Signal clk ActiveLow
                        , piaIRQB :: Signal clk ActiveLow
                        }
                deriving Show

pia :: forall clk. (Clock clk) => PIAIn clk -> PIAOut clk
pia PIAIn{..} = runRTL $ do
    let halfPIA (perif1, _perif2) isP isC = do
            irq1 <- newReg False
            irq2 <- newReg False
            ctrlLo <- newReg (0 :: U6)
            let ctrl = (unsigned (reg irq1) `shiftL` 7) .|.
                       (unsigned (reg irq2) `shiftL` 6) .|.
                       unsigned (reg ctrlLo)

            ddr <- newReg 0

            let rise1 = risingEdge perif1
                fall1 = fallingEdge perif1
            let watchRise1 = reg ctrlLo `testABit` 1

            WHEN ((watchRise1 .&&. rise1) .||. (bitNot watchRise1 .&&. fall1)) $ do
                irq1 := high

            WHEN cs . CASE $
              [ IF (bitNot we) $ do -- Read-triggered events
                     WHEN isP $ do
                         irq1 := low
                         irq2 := low
              , IF we $ do -- Write-triggered events
                     CASE [ IF isP . CASE $
                            [ IF (ctrl `testABit` 3) $ do
                                   return ()
                            , OTHERWISE $ do
                                   ddr := written
                            ]
                          , IF isC $ do
                                let b = unsigned written
                                    b' = mux (b `testABit` 5) (b, b .&. 0xbf)
                                ctrlLo := b'
                          ]
              ]

            let irq = bitNot $ risingEdge (reg irq1) .||. risingEdge (reg irq2)
            return (ctrl, ddr, irq)

    (ctrlA, ddrA, piaIRQA) <- halfPIA piaPerifA isPA isCA
    (ctrlB, ddrB, piaIRQB) <- halfPIA piaPerifB isPB isCB

    let piaR = memoryMapping [ (isPA, mux (ctrlA `testABit` 2) (reg ddrA, 0xF3))
                             , (isCA, ctrlA)
                             , (isPB, mux (ctrlB `testABit` 2) (reg ddrB, 0xFF))
                             , (isCB, ctrlB)
                             ]
    return PIAOut{..}
  where
    (cs, addr) = unpackEnabled piaA
    (we, written) = unpackEnabled piaW

    isPA = addr .==. 0x0
    isCA = addr .==. 0x1
    isPB = addr .==. 0x2
    isCB = addr .==. 0x3
