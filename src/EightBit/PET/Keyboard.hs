{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module EightBit.PET.Keyboard where

import MOS6502.Types

import Language.KansasLava
import Data.Sized.Unsigned as Unsigned

data KeyboardIn clk = KeyboardIn{ keyboardRowSel :: Signal clk U4 }
                    deriving Show

data KeyboardOut clk = KeyboardOut{ keyboardRow :: Signal clk Byte }
                     deriving Show

keyboard :: forall clk. (Clock clk) => KeyboardIn clk -> KeyboardOut clk
keyboard KeyboardIn{..} = KeyboardOut{..}
  where
    keyboardRow = 0xFF
