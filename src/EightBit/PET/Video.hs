{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module EightBit.PET.Video where

import MOS6502.Types

import Language.KansasLava
import Language.KansasLava.VHDL
import Language.Netlist.GenVHDL
import Hardware.KansasLava.Boards.Papilio
import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA.Driver
import Hardware.KansasLava.Xilinx.DCM

import Data.Sized.Ix
import Data.Sized.Unsigned as Unsigned

type VidX = U9
type VidY = U8

data TextIn clk = TextIn{ textFontRow :: Signal clk Byte
                        , textChar :: Signal clk Byte
                        }

data TextOut clk = TextOut{ textFontIdx :: Signal clk Byte
                          , textFontRowIdx :: Signal clk X8
                          , textCharIdx :: Signal clk U10
                          }

text40x25 :: forall clk. (Clock clk)
          => Signal clk (U4, U4, U4)
          -> TextIn clk
          -> (TextOut clk, VGADriverOut clk X6 X5 U4 U4 U4)
text40x25 color TextIn{..} = (TextOut{..}, VGADriverOut{vgaOutX = x', vgaOutY = y', ..})
  where
    mode = vga800x600at60
    VGADriverOut{..} = driveVGA (Witness :: Witness X1) mode (VGADriverIn r g b)

    (validX, x) = unpackEnabled vgaOutX
    (validY, y) = unpackEnabled vgaOutY

    (x', y') = (packEnabled inField cX, packEnabled inField cY)

    (cX, fontCol, cY, fontRow, newCol, _rowPhase) = runRTL $ do
        col <- newReg (0 :: X8)
        cX <- newReg (0 :: U6)
        row <- newReg (0 :: X8)
        cY <- newReg (0 :: U5)

        phase <- newReg False
        rowPhase <- newReg False

        WHEN vgaOutClkPhase $ do
            phase := bitNot (reg phase)
            WHEN (x .==. 0) $ do
                cX := 0
                col := 0
                WHEN (y .>. pureS yStart) $
                  rowPhase := bitNot $ reg rowPhase

                CASE [ IF (y .==. pureS yStart) $ do
                            cY := 0
                            row := 0
                            rowPhase := low
                     , IF (reg rowPhase .&&. inFieldV) $ do
                            row := reg row + 1
                            WHEN (reg row .==. pureS maxBound) $ do
                                cY := reg cY + 1
                     ]

            WHEN (reg phase .&&. inField) $ do
                col := reg col + 1
                WHEN (reg col .==. pureS maxBound) $ do
                    cX := reg cX + 1

        return (reg cX, reg col,
                reg cY, reg row,
                var phase,
                reg rowPhase)

    textFontRowIdx = fontRow

    newAddr = (x .==. 10) .||. (fontCol .==. maxBound .&&. newCol)
    newData = (x .==. 11) .||. (fontCol .==. maxBound .&&. bitNot newCol)

    (textCharIdx, textFontIdx, fontData) = runRTL $ do
        -- textCharIdx is always the *next* index
        let textVOffset = unsigned cY * 40
            textHOffset = unsigned cX + 1
        let textCharIdx = mux (y .>=. pureS yStart)
                          (0,
                           textVOffset + mux (x .>=. pureS xStart) (0, textHOffset))

        -- On last column, load next character...
        charBuffer <- newReg 0
        WHEN newAddr $ do
            charBuffer := textChar

        -- ... and store the next row
        rowBuffer <- newReg 0
        WHEN newData $ do
            rowBuffer := textFontRow

        return (textCharIdx, var charBuffer, reg rowBuffer)

    pixel = fontData `testABit` (7 - fontCol)
    rgb = mux pixel (pureS minBound, color)
    rgb' = mux (low .||. inField) (pureS (2, 2, 2), rgb)

    wReal = visibleSize . vgaHorizTiming $ mode
    hReal = visibleSize . vgaVertTiming $ mode
    wVirt = 40 * 8 * 2
    hVirt = 25 * 8 * 2
    xStart = (wReal - wVirt) `div` 2
    yStart = (hReal - hVirt) `div` 2
    xEnd = xStart + wVirt
    yEnd = yStart + hVirt

    inFieldH = validX .&&. x `betweenCO` (xStart, xEnd)
    inFieldV = validY .&&. y `betweenCO` (yStart, yEnd)
    inField = inFieldH .&&. inFieldV

    (r, g, b') = unpack rgb'
    b = funMap (Just . fixBlue) b'

fixBlue :: U4 -> U4
fixBlue b = sorted !! fromIntegral b
  where
    sorted = [ 0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x8, 0x6
             , 0xa, 0x7, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf
             ]

betweenCO :: (Ord a, Rep a) => Signal clk a -> (a, a) -> Signal clk Bool
x `betweenCO` (lo, hi) = pureS lo .<=. x .&&. x .<. pureS hi

synthesize :: Model -> String -> Fabric () -> IO (String, String, [String])
synthesize model modName bench = do
    kleg <- reifyFabric $ do
        theClk clock
        wing_init
        bench

    mod <- netlistCircuit modName kleg
    let mod' = dcm40MHz mod
        vhdl = genVHDL mod' ["work.lava.all", "work.all"]

    ucf <- toUCF model kleg

    return (vhdl, ucf, [dcmName])
  where
    clock = "CLK_40MHZ"
    dcmName = "dcm_32_to_40"
    dcm40MHz = dcm dcmName "CLK_32MHZ" clock
