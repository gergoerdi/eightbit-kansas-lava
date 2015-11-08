{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module EightBit.PET.PIA where

import MOS6502.Types
import MOS6502.Utils

import Language.KansasLava

import Data.Sized.Unsigned
import Data.Bits

data PIAIn clk = PIAIn{ piaA :: Signal clk (Enabled U2)
                      , piaW :: Signal clk (Enabled Byte)
                      , piaTriggerA, piaTriggerB :: (Signal clk Bool, Signal clk Bool)
                      , piaInputA, piaInputB :: Signal clk Byte
                      }
               deriving Show

data PIAOut clk = PIAOut{ piaR :: Signal clk Byte
                        , piaIRQA, piaIRQB :: Signal clk ActiveLow
                        , piaOutputA, piaOutputB :: Signal clk Byte
                        }
                deriving Show

pia :: forall clk. (Clock clk) => PIAIn clk -> PIAOut clk
pia PIAIn{..} = runRTL $ do
    let halfPIA (trigger1, _trigger2) isP isC perif = do
            irq1 <- newReg False
            irq2 <- newReg False
            ctrlLo <- newReg (0 :: U6)
            let ctrl = (unsigned (reg irq1) `shiftL` 7) .|.
                       (unsigned (reg irq2) `shiftL` 6) .|.
                       unsigned (reg ctrlLo)
                targetPerif = ctrl `testABit` 3

            ddr <- newReg (0 :: Byte)
            dat <- newReg (0 :: Byte)
            let perif' = (perif .&. complement (reg ddr)) .|. (reg dat .&. reg ddr)
                port = mux targetPerif (reg ddr, perif')

            let rise1 = risingEdge trigger1
                fall1 = fallingEdge trigger1
            let watchRise1 = reg ctrlLo `testABit` 1
                signal1 = mux watchRise1 (fall1, rise1)

            WHEN signal1 $ do
                irq1 := high

            WHEN cs . CASE $
              [ IF (bitNot we) $ do -- Read-triggered events
                     WHEN isP $ do
                         irq1 := low
                         irq2 := low
              , IF we $ do -- Write-triggered events
                     CASE [ IF isP . CASE $
                            [ IF targetPerif $ do
                                   dat := written
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
            return (port, ctrl, irq, reg dat)

    (portA, ctrlA, piaIRQA, piaOutputA) <- halfPIA piaTriggerA isPA isCA piaInputA
    (portB, ctrlB, piaIRQB, piaOutputB) <- halfPIA piaTriggerB isPB isCB piaInputB

    let piaR = memoryMapping [ (isPA, portA)
                             , (isCA, ctrlA)
                             , (isPB, portB)
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
