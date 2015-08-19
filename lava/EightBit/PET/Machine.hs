{-# LANGUAGE RecordWildCards #-}
module EightBit.PET.Machine (machine, injectKeyboard) where

import MOS6502.Types
import MOS6502.CPU (CPUIn(..), CPUOut(..), CPUDebug(..))
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
import Control.Monad (guard)

injectKeyboard :: (Clock clk)
               => [U8] -> Signal clk Bool
               -> Signal clk (Enabled (Bool, U8))
               -> Signal clk (Enabled (Bool, U8))
injectKeyboard keys next s = runRTL $ do
    wait <- newReg (0 :: X10)
    let finishedWaiting = reg wait .==. pureS maxBound
    WHEN (bitNot finishedWaiting .&&. next) $ do
        wait := reg wait + 1
    let next' = finishedWaiting .&&. next

    i <- newReg (0 :: U8)
    let finished = reg i .==. pureS n
    WHEN (bitNot finished .&&. next') $ do
        i := reg i + 1

    let keycode = rom (reg i) $ \i -> do
            guard $ i < n
            return $ keys !! fromIntegral i
    let key = pack (delay $ bitNot next', keycode)
    return $ mux (delay finished) (packEnabled finishedWaiting key, s)
  where
    n = fromIntegral (length keys) :: U8

machine :: ByteString -> ByteString -> (Fabric (), Fabric ())
machine fontImage kernalImage = (main, video)
  where
    main = do
        theClk "CLK_1MHZ"
        theRst "RESET"

        (ps2A, _) <- ps2
        let realKeyboardEvent = eventPS2 . decodePS2 . samplePS2 $ ps2A
        -- let realKeyboardEvent = disabledS

        vblank <- inStdLogic "VBLANK"
        vRead <- inStdLogicVector "VRAM_DOUT"
        let (keyboardRowSel, (vAddr, vWrite), (CPUIn{..}, CPUOut{..}, CPUDebug{..})) =
                  board kernalImage vblank keyboardRow vRead
            keyboardEvent = const realKeyboardEvent $ injectKeyboard initialInput vblank realKeyboardEvent
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

    initialInput = [ 0x69, 0x29, 0x43, 0x77, 0x43, 0x79, 0x69, 0x5A -- 1 I=I+1
                   , 0x72, 0x29, 0x4D, 0x2D, 0x43, 0x31, 0x2C, 0x29, 0x43, 0x5A -- 2 PRINT I
                   , 0x7A, 0x29, 0x34, 0x44, 0x2C, 0x44, 0x29, 0x69, 0x5A -- 3 GOTO 1
                   , 0x2D, 0x3C, 0x31, 0x5A -- RUN
                   ]

    video = do
        theClk "CLK_40MHZ"
        theRst "RESET"

        let color = pureS (0xF, 0xF, 0xF)
        -- palette = Matrix.fromList [ (0x0, 0xF, 0x0)
        --                           , (0xF, 0xF, 0xF)
        --                           , (0xF, 0xB, 0x0)
        --                           , (0x0, 0x8, 0xF)
        --                           ]

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
