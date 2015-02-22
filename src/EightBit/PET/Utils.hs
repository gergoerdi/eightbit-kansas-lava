{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EightBit.PET.Utils (forceDefined) where

import Language.KansasLava
import Language.KansasLava.Signal
import Control.Applicative

forceDefined :: (Clock clk, Rep a) => a -> Signal clk a -> Signal clk a
forceDefined def = shallowMapS (fmap (optX . (<|> Just def) . unX))
