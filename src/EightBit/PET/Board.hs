{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EightBit.PET.Board (board) where

import MOS6502.Types
import MOS6502.CPU
import EightBit.PET.Video

import Language.KansasLava
import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA.Driver
import Hardware.KansasLava.VGA

import Data.Sized.Unsigned
import Data.Sized.Matrix
import qualified Data.Sized.Matrix as Matrix

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

loadFont :: BS.ByteString -> U11 -> Byte
loadFont font = fromIntegral . BS.index font . fromIntegral

board :: ByteString -> ByteString -> ByteString -> Fabric ()
board fontImage kernalImage basicImage = do
    vga . encodeVGA . vgaOut $ boardCircuit fontImage kernalImage basicImage

fromImage :: (Integral a) => ByteString -> a -> Byte
fromImage bs addr | addr' < BS.length bs = fromIntegral . BS.index bs $ addr'
                  | otherwise = 0x00
  where
    addr' = fromIntegral addr

memoryMapping :: (Clock clk, Rep a)
              => Signal clk a
              -> [(Signal clk Bool, Signal clk a)]
              -> Signal clk a
memoryMapping = foldr (\(sel, v) sig -> mux sel (v, sig))

boardCircuit :: forall clk. (Clock clk)
             => ByteString
             -> ByteString
             -> ByteString
             -> VGADriverOut clk X6 X5 U4 U4 U4
boardCircuit fontImage kernalImage basicImage = vga
  where
    (TextOut{..}, vga) = text40x25 (pureS maxBound) TextIn{..}
    (CPUOut{..}, _cpuDebug) = cpu CPUIn{..}

    cpuIRQ = high
    cpuNMI = high

    -- Slow down CPU 1024-fold
    cpuWait = runRTL $ do
        counter <- newReg (0 :: U10)
        counter := reg counter + 1
        return $ reg counter ./=. 0

    page = delay $ cpuMemA `shiftR` 12

    vAddr :: Signal clk U10
    vAddr = unsigned cpuMemA

    isVideo = page .==. 0x8
    vPipe = packEnabled (isEnabled cpuMemW .&&. isVideo) $
            pack (vAddr, enabledVal cpuMemW)
    vRAM = writeMemory vPipe
    vRead = syncRead vRAM vAddr

    mAddr :: Signal clk U13
    mAddr = unsigned cpuMemA

    isMemory = page .<=. 0x3
    mPipe = packEnabled (isEnabled cpuMemW .&&. isMemory) $
            pack (mAddr, enabledVal cpuMemW)
    mRAM = writeMemory mPipe
    mRead = syncRead mRAM mAddr

    basicAddr :: Signal clk U13
    basicAddr = unsigned cpuMemA
    isBasic = 0xC .<=. page .&&. page .<=. 0xD
    basicROM = rom basicAddr (Just . fromImage basicImage)

    kernalAddr :: Signal clk U12
    kernalAddr = unsigned cpuMemA
    isKernal = page .==. 0xF
    kernalROM = rom kernalAddr (Just . fromImage kernalImage)

    cpuMemR = memoryMapping 0
              [ (isKernal, kernalROM)
              , (isBasic, basicROM)
              , (isVideo, vRead)
              , (isMemory, mRead)
              ]

    fontAddr :: Signal clk U11
    fontAddr = unsigned (textFontIdx .&. 0x7F) `shiftL` 3 + unsigned textFontRowIdx
    textFontRow = invertFont (textFontIdx `testABit` 7) $
                  rom fontAddr (Just . fromImage fontImage)
    textChar = syncRead vRAM textCharIdx

invertFont :: (Clock clk)
           => Signal clk Bool -> Signal clk Byte -> Signal clk Byte
invertFont b = xor mask
  where
    mask = mux b (0, 0xff)
