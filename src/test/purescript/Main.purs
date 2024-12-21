module Test.Main where

import Prelude

import Effect (Effect)
import Test.JoeOp.Calendar.Data.Date as Calendar.Data.Date

main :: Effect Unit
main = do
  Calendar.Data.Date.addMonths
