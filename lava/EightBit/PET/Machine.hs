{-# LANGUAGE RecordWildCards #-}
module EightBit.PET.Machine (machine) where

import MOS6502.Types
import MOS6502.CPU (CPUIn(..), CPUOut(..), CPUDebug(..))
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

machine :: ByteString -> ByteString -> (Fabric (), Fabric ())
machine fontImage kernalImage = (main, video)
  where
    main = do
        theClk "CLK_1MHZ"
        theRst "RESET"

        (ps2A, _) <- ps2
        let keyboardEvent = eventPS2 . decodePS2 . samplePS2 $ ps2A

        vblank <- inStdLogic "VBLANK"
        vRead <- inStdLogicVector "VRAM_DOUT"
        let (keyboardRowSel, (vAddr, vWrite), (CPUIn{..}, CPUOut{..}, CPUDebug{..})) =
                  board kernalImage vblank keyboardRow vRead
            KeyboardOut{..} = keyboard KeyboardIn{..}

        -- Debug output
        outStdLogicVector "PC" cpuPC
        outStdLogicVector "ADDR" cpuMemA
        outStdLogicVector "STATE" cpuState
        outStdLogicVector "READ" cpuMemR
        outStdLogicVector "WRITE" cpuMemW
        outStdLogicVector "FLAGS" cpuP
        outStdLogicVector "SP" cpuSP
        outStdLogicVector "A" cpuA
        outStdLogicVector "X" cpuX
        outStdLogicVector "Y" cpuY
        outStdLogicVector "KEYBOARD" keyboardEvent

        let (vram_we, vram_write_data) = unpackEnabled vWrite
            vram_addr = vAddr
        outStdLogic "VRAM_WE" vram_we
        outStdLogicVector "VRAM_DIN" vram_write_data
        outStdLogicVector "VRAM_ADDR" vram_addr

    video = do
        theClk "CLK_40MHZ"
        theRst "RESET"

        let color = pureS (0xF, 0xF, 0xF)
        textChar <- inStdLogicVector "VRAM_DOUT"

        let vblank = vgaOutVBlank video
            (TextOut{..}, video) = text40x25 color TextIn{..}

            fontAddr :: Signal CLK U11
            fontAddr = unsigned (textFontIdx .&. 0x7F) `shiftL` 3 + unsigned textFontRowIdx

            textFontRow = invertFont (textFontIdx `testABit` 7) $
                          rom fontAddr (Just . fromImage fontImage)

        outStdLogic "VBLANK" vblank
        outStdLogicVector "VRAM_ADDR" textCharIdx
        vga . encodeVGA . vgaOut $ video

invertFont :: (Clock clk)
           => Signal clk Bool -> Signal clk Byte -> Signal clk Byte
invertFont b = xor mask
  where
    mask = mux b (0, 0xff)
