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
                      , piaTriggerA :: (Signal clk Bool, Signal clk Bool)
                      , piaTriggerB :: (Signal clk Bool, Signal clk Bool)
                      }
               deriving Show

data PIAOut clk = PIAOut{ piaR :: Signal clk Byte
                        , piaIRQA :: Signal clk ActiveLow
                        , piaIRQB :: Signal clk ActiveLow
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

            WHEN ((watchRise1 .&&. rise1) .||. (bitNot watchRise1 .&&. fall1)) $ do
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
            return (port, ctrl, irq)

    (portA, ctrlA, piaIRQA) <- halfPIA piaTriggerA isPA isCA 0xFF
    (portB, ctrlB, piaIRQB) <- halfPIA piaTriggerB isPB isCB 0xFF

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
