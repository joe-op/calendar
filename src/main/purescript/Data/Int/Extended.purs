module Data.Int.Extended
  ( abs
  ) where

import Prelude

abs :: Int -> Int
abs n =
  if n >= 0 then
    n
  else
    (-1) * n

