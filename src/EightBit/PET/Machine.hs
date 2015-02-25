{-# LANGUAGE RecordWildCards #-}
module EightBit.PET.Machine (machine) where

import MOS6502.Types
import EightBit.PET.Board
import EightBit.PET.Video
import EightBit.PET.Keyboard

import Language.KansasLava
import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA.Driver
import Hardware.KansasLava.VGA
import Hardware.KansasLava.PS2

import Data.ByteString (ByteString)
import Data.Sized.Unsigned
import Data.Bits

machine :: ByteString -> ByteString -> Fabric ()
machine fontImage kernalImage = do
    (ps2A, _) <- ps2
    let keyboardEvent = eventPS2 . decodePS2 . samplePS2 $ ps2A

    let (textRAM, keyboardRowSel, _) = board kernalImage (vgaOutVBlank video) keyboardRow
        (TextOut{..}, video) = text40x25 (pureS maxBound) TextIn{..}
        KeyboardOut{..} = keyboard KeyboardIn{..}

        fontAddr :: Signal CLK U11
        fontAddr = unsigned (textFontIdx .&. 0x7F) `shiftL` 3 + unsigned textFontRowIdx

        textFontRow = invertFont (textFontIdx `testABit` 7) $
                      rom fontAddr (Just . fromImage fontImage)

        textChar = syncRead textRAM textCharIdx

    vga . encodeVGA . vgaOut $ video

invertFont :: (Clock clk)
           => Signal clk Bool -> Signal clk Byte -> Signal clk Byte
invertFont b = xor mask
  where
    mask = mux b (0, 0xff)
