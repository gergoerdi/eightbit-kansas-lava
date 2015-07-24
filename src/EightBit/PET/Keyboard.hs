{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module EightBit.PET.Keyboard where

import MOS6502.Types
import MOS6502.Utils

import Prelude hiding (sequence)
import Control.Applicative ((<$>))
import Data.Traversable (sequence)

import Language.KansasLava
import Data.Sized.Unsigned
import Data.Sized.Ix
import Data.Sized.Matrix ((!))
import qualified Data.Sized.Matrix as Matrix
import Data.Bits

data KeyboardIn clk = KeyboardIn{ keyboardEvent :: Signal clk (Enabled (Bool, U8))
                                , keyboardRowSel :: Signal clk X10 }
                    deriving Show

data KeyboardOut clk = KeyboardOut{ keyboardRow :: Signal clk Byte }
                     deriving Show

keyboard :: forall clk. (Clock clk) => KeyboardIn clk -> KeyboardOut clk
keyboard KeyboardIn{..} = runRTL $ do
    rowRegs <- sequence $ Matrix.forAll $ \_ -> newReg 0
    let rows = reg <$> rowRegs

    CASE [ match (translateKeyboard keyboardEvent) $ \(unpack -> (pressed, row, col)) -> do
                let mask = muxN [ (col .==. pureS i, 0x01 `shiftL` fromIntegral i)
                                | i <- [0..7]
                                ]
                    toggle x = mux pressed (x .&. complement mask, x .|. mask)

                CASE [ IF (row .==. pureS i) $ do
                            (rowRegs ! i) := toggle (rows ! i)
                     | i <- [minBound..maxBound]
                     ]
         ]

    let keyboardRow = packMatrix rows .!. keyboardRowSel
    return KeyboardOut{..}

translateKeyboard :: (Clock clk)
                  => Signal clk (Enabled (Bool, U8))
                  -> Signal clk (Enabled (Bool, X10, X8))
translateKeyboard keyboardEvent =
    packEnabled (delay (isEnabled keyboardEvent) .&&. isValid) $
    pack (delay pressed, row, col)
  where
    (pressed, scancode) = unpack $ enabledVal keyboardEvent
    (isValid, unpack -> (row, col)) = unpackEnabled $ rom scancode (return . scancodes)

scancodes :: U8 -> Maybe (X10, X8)
scancodes 0x70 = return (8, 6) -- 0
scancodes 0x69 = return (6, 6) -- 1
scancodes 0x72 = return (7, 6) -- 2
scancodes 0x7A = return (6, 7) -- 3
scancodes 0x6B = return (4, 6) -- 4
scancodes 0x73 = return (5, 6) -- 5
scancodes 0x74 = return (4, 7) -- 6
scancodes 0x6C = return (2, 6) -- 7
scancodes 0x75 = return (3, 6) -- 8
scancodes 0x7D = return (2, 7) -- 9

scancodes 0x1C = return (4, 0) -- A
scancodes 0x32 = return (6, 2) -- B
scancodes 0x21 = return (6, 1) -- C
scancodes 0x23 = return (4, 1) -- D
scancodes 0x24 = return (2, 1) -- E
scancodes 0x2B = return (5, 1) -- F
scancodes 0x34 = return (4, 2) -- G
scancodes 0x33 = return (5, 2) -- H
scancodes 0x43 = return (3, 3) -- I
scancodes 0x3B = return (4, 3) -- J
scancodes 0x42 = return (5, 3) -- K
scancodes 0x4B = return (4, 4) -- L
scancodes 0x3A = return (6, 3) -- M
scancodes 0x31 = return (7, 2) -- N
scancodes 0x44 = return (2, 4) -- O
scancodes 0x4D = return (3, 4) -- P
scancodes 0x15 = return (2, 0) -- Q
scancodes 0x2D = return (3, 1) -- R
scancodes 0x1B = return (5, 0) -- S
scancodes 0x2C = return (2, 2) -- T
scancodes 0x3C = return (2, 3) -- U
scancodes 0x2A = return (7, 1) -- V
scancodes 0x1D = return (3, 0) -- W
scancodes 0x22 = return (7, 0) -- X
scancodes 0x35 = return (3, 2) -- Y
scancodes 0x1A = return (6, 0) -- Z

scancodes 0x29 = return (9, 2) -- SPACE
scancodes 0x5A = return (6, 5) -- ENTER
scancodes 0x66 = return (1, 7) -- BACKSPACE
scancodes 0x52 = return (9, 4) -- RUN/STOP (')

scancodes 0x1E = return (1, 0) -- "
scancodes 0x16 = return (0, 0) -- !
scancodes 0x77 = return (9, 7) -- = (num lock)
scancodes 0x79 = return (7, 7) -- + (KP +)

scancodes _    = Nothing

-- TODO: This doesn't work for multi-byte scancodes
eventPS2 :: (Clock clk)
         => Signal clk (Enabled U8) -> Signal clk (Enabled (Bool, U8))
eventPS2 scancode = runRTL $ do
    releasing <- newReg False
    lastKey <- newReg Nothing

    CASE [ match scancode $ \code -> do
                CASE [ IF (reg releasing) $ do
                            releasing := low
                     , IF (code .==. 0xF0) $ do
                            releasing := high
                     ]
                lastKey := enabledS $ pack (bitNot (reg releasing), code)
         , OTHERWISE $ do
                lastKey := disabledS
         ]

    return $ reg lastKey
