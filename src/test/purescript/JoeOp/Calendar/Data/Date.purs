module Test.JoeOp.Calendar.Data.Date
  ( addMonths
  ) where

import Prelude
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import JoeOp.Calendar (Month, Year(..))
import JoeOp.Calendar.Month as Month
import JoeOp.Calendar.Data.Date as Date
import Test.QuickCheck (quickCheck)

jan25 :: Tuple Year Month
jan25 = Tuple (Year 2025) Month.January

addMonths :: Effect Unit
addMonths = do
  quickCheck $ \n -> addMonthsTupled (addMonthsTupled jan25 n) ((-1) * n) == jan25
  quickCheck $ \n ->
    let
      result = addMonthsTupled jan25 n
    in
      fst result == Year (2025 + (n - (mod n 12)) / 12)
  quickCheck $ \n ->
    let
      result = addMonthsTupled jan25 n

      jan25months = 2025 * 12 + 1
      resultmonths = (unwrap $ fst result) * 12 + Month.toCardinalInt (snd result)
    in
      jan25months + n == resultmonths
  where
  addMonthsTupled :: Tuple Year Month -> Int -> Tuple Year Month
  addMonthsTupled ym = Date.addMonths (fst ym) (snd ym)
