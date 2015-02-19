{-# LANGUAGE RecordWildCards #-}
module EightBit.PET.Machine (machine) where

import MOS6502.Types
import EightBit.PET.Board
import EightBit.PET.Video

import Language.KansasLava
import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA.Driver
import Hardware.KansasLava.VGA

import Data.ByteString (ByteString)
import Data.Sized.Unsigned
import Data.Bits

machine :: ByteString -> ByteString -> ByteString -> Fabric ()
machine fontImage kernalImage basicImage = do
    vga . encodeVGA . vgaOut $ video
  where
    (TextOut{..}, video) = text40x25 (pureS maxBound) TextIn{..}
    (textRAM, _) = board kernalImage basicImage

    fontAddr :: Signal CLK U11
    fontAddr = unsigned (textFontIdx .&. 0x7F) `shiftL` 3 + unsigned textFontRowIdx

    textFontRow = invertFont (textFontIdx `testABit` 7) $
                  rom fontAddr (Just . fromImage fontImage)

    textChar = syncRead textRAM textCharIdx

invertFont :: (Clock clk)
           => Signal clk Bool -> Signal clk Byte -> Signal clk Byte
invertFont b = xor mask
  where
    mask = mux b (0, 0xff)
