module Test.JoeOp.Calendar.Data.Date
  ( addMonths
  ) where

import Prelude
import Effect (Effect)
import Test.QuickCheck (quickCheck)

-- TODO: test
addMonths :: Effect Unit
addMonths = quickCheck $ \n -> n + 1 - 1 == n
