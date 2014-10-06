{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module EightBit.PET.Video where

import MOS6502.Types
import EightBit.PET.DCM

import Language.KansasLava
import Language.KansasLava.VHDL
import Language.Netlist.GenVHDL
import Hardware.KansasLava.Boards.Papilio
import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA.Driver

import Data.Sized.Unsigned as Unsigned
import Data.Sized.Matrix
import Data.Bits

-- import System.FilePath
-- import System.Directory

type VidX = U9
type VidY = U8

data TextIn clk = TextIn{ textFont :: Signal clk (Matrix X8 Byte)
                        , textChar :: Signal clk Byte
                        }

data TextOut clk = TextOut{ textFontIdx :: Signal clk Byte
                          , textCharIdx :: Signal clk U10
                          }

text40x25 :: (Clock clk)
          => Signal clk (U4, U4, U4)
          -> TextIn clk
          -> (TextOut clk, VGADriverOut clk X6 X5 U4 U4 U4)
text40x25 color TextIn{..} = (TextOut{..}, VGADriverOut{vgaOutX = x', vgaOutY = y', ..})
  where
    mode = vga800x600at60
    VGADriverOut{..} = driveVGA mode (VGADriverIn r g b)

    (validX, x) = unpackEnabled vgaOutX
    (validY, y) = unpackEnabled vgaOutY

    (x', y') = (packEnabled inField cX, packEnabled inField cY)

    (cX, fontCol, cY, fontRow) = runRTL $ do
        col <- newReg (0 :: X8)
        cX <- newReg (0 :: U6)
        row <- newReg (0 :: X8)
        cY <- newReg (0 :: U5)

        phase <- newReg False
        rowPhase <- newReg False

        WHEN vgaOutClkPhase $ do
            phase := bitNot (reg phase)
            WHEN (reg phase) $ do
                WHEN (x .==. pureS (xEnd + 1)) $ do
                    cX := 0
                    col := 0
                    rowPhase := bitNot $ reg rowPhase

                    CASE [ IF (y .==. 0) $ do
                                cY := 0
                                row := 0
                         , IF (reg rowPhase .&&. inFieldV) $ do
                                row := reg row + 1
                                WHEN (reg row .==. pureS maxBound) $ do
                                    cY := reg cY + 1
                         ]

                WHEN inField $ do
                    col := reg col + 1
                    WHEN (reg col .==. pureS maxBound) $ do
                        cX := reg cX + 1

        return (reg cX, reg col,
                reg cY, reg row)

    textCharIdx = unsigned cY * 40 + unsigned cX

    textFontIdx = runRTL $ do
        r <- newReg 0
        WHEN (inField .&&. vgaOutClkPhase .&&. fontCol .==. 0) $ do
            r := textChar
        return $ var r

    pixel = (textFont .!. fontRow) `testABit` (7 - fontCol)
    -- pixel = textCharIdx .==. pureS (5 * 40)
    -- pixel = fontCol .==. 0
    -- pixel = cY .==. 5 .&&. cX .==. 0
    rgb = mux inField (pureS (2, 2, 2),
                       -- pack (mux (fontRow .==. 0) (pureS minBound, pureS maxBound),
                       --       mux (y .==. pureS yStart) (pureS minBound, pureS maxBound),
                       --       minBound))
                       mux pixel (pureS minBound, color))
    -- rgb = mux pixel (pureS minBound, color)

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

    -- rgb = mux inField (pureS (2, 2, 2), mux pixel (pureS minBound, color))
    (r, g, b') = unpack rgb
    b = funMap (Just . fixBlue) b'

fixBlue :: U4 -> U4
fixBlue b = sorted !! fromIntegral b
  where
    sorted = [ 0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x8, 0x6
             , 0xa, 0x7, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf
             ]

betweenCO :: (Ord a, Rep a) => Signal clk a -> (a, a) -> Signal clk Bool
x `betweenCO` (lo, hi) = pureS lo .<=. x .&&. x .<. pureS hi

synthesize :: Model -> String -> Fabric () -> IO (String, String)
synthesize model modName bench = do
    kleg <- reifyFabric $ do
        theClk clock
        wing_init
        bench

    mod <- netlistCircuit modName kleg
    let mod' = dcm80MHz clock mod
        vhdl = genVHDL mod' ["work.lava.all", "work.all"]

    ucf <- toUCF model kleg

    return (vhdl, ucf)
  where
    clock = "CLK_80MHZ"
