{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EightBit.PET.Board (board, fromImage) where

import MOS6502.Types
import MOS6502.CPU

import Language.KansasLava
import Language.KansasLava.Signal

import Data.Sized.Unsigned
import Control.Applicative
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

fromImage :: (Integral a) => ByteString -> a -> Byte
fromImage bs addr | addr' < BS.length bs = fromIntegral . BS.index bs $ addr'
                  | otherwise = 0x00
  where
    addr' = fromIntegral addr

memoryMapping :: (Clock clk, Rep a)
              => [(Signal clk Bool, Signal clk a)]
              -> Signal clk a
memoryMapping = foldr (\(sel, v) sig -> mux (delay sel) (sig, v)) undefinedS

forceDefined :: (Clock clk, Rep a) => a -> Signal clk a -> Signal clk a
forceDefined def = shallowMapS (fmap (optX . (<|> Just def) . unX))

board :: forall clk. (Clock clk)
      => ByteString
      -> ByteString
      -> (Signal clk (U10 -> Byte), (CPUIn clk, CPUOut clk, CPUDebug clk))
board kernalImage basicImage = (vRAM, (cpuIn, cpuOut, cpuDebug))
  where
    cpuIn = CPUIn{..}
    (cpuOut@CPUOut{..}, cpuDebug) = cpu cpuIn

    cpuIRQ = high
    cpuNMI = high

    -- Slow down CPU 50-fold
    -- cpuWait = runRTL $ do
    --     counter <- newReg (0 :: Byte)
    --     counter := mux (reg counter .==. 50) (reg counter + 1, 0)
    --     return $ reg counter ./=. 0
    cpuWait = low

    page = cpuMemA `shiftR` 12

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

    cpuMemR = forceDefined 0x20 $
              memoryMapping
              [ (isKernal, kernalROM)
              , (isBasic, basicROM)
              , (isVideo, vRead)
              , (isMemory, mRead)
              ]
