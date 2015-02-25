{-# LANGUAGE RecordWildCards #-}
module EightBit.PET.Machine (machine) where

import MOS6502.Types
import EightBit.PET.Board
import EightBit.PET.Video
import EightBit.PET.Keyboard

import Language.KansasLava
import Language.KansasLava.Signal.Utils (debounce)
import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA.Driver
import Hardware.KansasLava.VGA
import Hardware.KansasLava.PS2

import Data.ByteString (ByteString)
import Data.Sized.Unsigned
import Data.Sized.Ix
import Data.Sized.Matrix ((!))
import qualified Data.Sized.Matrix as Matrix
import Data.Bits

machine :: ByteString -> ByteString -> Fabric ()
machine fontImage kernalImage = do
    (ps2A, _) <- ps2
    let keyboardEvent = eventPS2 . decodePS2 . samplePS2 $ ps2A

    (buttonUp, buttonDown) <- do
        Buttons{..} <- buttons
        let (_, buttonUp', _) = debounce (Witness :: Witness X16) buttonUp
            (_, buttonDown', _) = debounce (Witness :: Witness X16) buttonDown
        return (buttonUp', buttonDown')

    let palette = Matrix.fromList [ (0x0, 0xF, 0x0)
                                  , (0xF, 0xF, 0xF)
                                  , (0xF, 0xB, 0x0)
                                  , (0x0, 0x8, 0xF)
                                  ]
        color = funMap (Just . (palette !)) $ runRTL $ do
            r <- newReg (0 :: U2)
            CASE [ IF buttonUp $ r := reg r + 1
                 , IF buttonDown $ r := reg r - 1
                 ]
            return $ reg r

    let (textRAM, keyboardRowSel, _) = board kernalImage (vgaOutVBlank video) keyboardRow
        (TextOut{..}, video) = text40x25 color TextIn{..}
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
