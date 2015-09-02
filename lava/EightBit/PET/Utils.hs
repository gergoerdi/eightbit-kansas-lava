{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EightBit.PET.Utils
       ( forceDefined
       , bus, unbus
       ) where

import Language.KansasLava
import Language.KansasLava.Signal
import Data.Sized.Matrix
import Data.Sized.Unsigned
import Control.Applicative

forceDefined :: (Clock clk, Rep a) => a -> Signal clk a -> Signal clk a
forceDefined def = shallowMapS (fmap (optX . (<|> Just def) . unX))

bus :: (Size n) => Matrix n (Signal clk Bool) -> Signal clk (Unsigned n)
bus = bitwise . packMatrix

unbus :: (Size n) => Signal clk (Unsigned n) -> Matrix n (Signal clk Bool)
unbus = unpackMatrix . bitwise
