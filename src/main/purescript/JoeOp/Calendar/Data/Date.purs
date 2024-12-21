module JoeOp.Calendar.Data.Date where

import Prelude
import Data.Int.Extended as Int
import Data.Maybe (fromMaybe)
import Data.Newtype (over)
import Data.Tuple.Nested (tuple3)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import JoeOp.Calendar (Date, Day(..), Month(..), UnwrappedDate, Year(..))
import JoeOp.Calendar.Date as Date
import JoeOp.Calendar.Month as Month

wrappedDate :: UnwrappedDate -> Date
wrappedDate { year, month, day } = tuple3 (Year year) (fromMaybe January (Month.fromCardinalInt month)) (Day day)

-- TODO: change this to "next month" etc.
-- TODO: set to day 1
unsafeTodayPlusMonths :: Int -> Effect Date
unsafeTodayPlusMonths months =
  Date.today <#>
    ( \today ->
        wrappedDate (today { month = 1 + mod (today.month + months - 1) 12 })
    )

unsafeToday :: Effect Date
unsafeToday = Date.today <#> wrappedDate

addMonths :: Year -> Month -> Int -> Tuple Year Month
addMonths year month n =
  let
    absN = Int.abs n
    signModifier = n / absN

    monthsToAdd = signModifier * (mod absN 12)

    monthRotationYearAdjustment :: Int
    monthRotationYearAdjustment = case monthsToAdd + Month.toIndex month of
      unrotatedNewMonthIndex
        | unrotatedNewMonthIndex >= 12 -> 1
        | unrotatedNewMonthIndex < 0 -> -1
        | otherwise -> 0

    yearsToAdd = signModifier * (absN / 12) + monthRotationYearAdjustment
  in
    Tuple
      (over Year ((+) yearsToAdd) year)
      (Month.addMod monthsToAdd month)

