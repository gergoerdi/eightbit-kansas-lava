module EightBit.PET.Machine (machine) where

import EightBit.PET.Board

import Language.KansasLava
import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA.Driver
import Hardware.KansasLava.VGA

import Data.ByteString (ByteString)

machine :: ByteString -> ByteString -> ByteString -> Fabric ()
machine fontImage kernalImage basicImage = do
    vga . encodeVGA . vgaOut . fst $ board fontImage kernalImage basicImage
