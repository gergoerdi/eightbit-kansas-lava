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
import qualified Data.ByteString as BS

type TextAddr = U10

loadFont :: BS.ByteString -> (Byte -> Matrix X8 Byte)
loadFont font addr = Matrix.fromList . map fromIntegral . BS.unpack . BS.take 8 . BS.drop (fromIntegral addr * 8) $ font

loadText :: BS.ByteString -> (TextAddr -> Byte)
loadText text addr | addr' < BS.length text = fromIntegral . BS.index text $ addr'
                   | otherwise = 0x20
  where
    addr' = fromIntegral addr

board :: BS.ByteString -> BS.ByteString -> Fabric ()
board font text = do
    vga . encodeVGA $ vgaOut
  where
    (TextOut{..}, VGADriverOut{..}) = text40x25 (pureS maxBound) TextIn{..}

    textFont = invertFont (textFontIdx `testABit` 7) $ rom textFontIdx (Just . loadFont font)
    textChar0 = rom textCharIdx (Just . loadText text)
    textChar = mux (textCharIdx .==. 5 * 40 .&&. flag) (textChar0, textChar0 .|. 0x80)

    flag = runRTL $ do
        r <- newReg (0 :: U32)
        r := mux (reg r .==. pureS (60 * 1000 * 1000)) (reg r + 1, 0)

        b <- newReg False
        WHEN (reg r .==. 0) $
            b := bitNot $ reg b
        return $ reg b

-- boardCircuit :: (ROMAddr

invertFont :: (Clock clk)
           => Signal clk Bool -> Signal clk (Matrix X8 Byte) -> Signal clk (Matrix X8 Byte)
invertFont b = packMatrix . fmap (xor mask) . unpackMatrix
  where
    mask = mux b (0, 0xff)
