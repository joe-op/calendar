module JoeOp.Calendar.Date
  ( Date
  , UnwrappedDate
  , monthStartsOnWeekDay
  , today
  ) where

import Data.Newtype (unwrap)
import Data.Tuple.Nested (Tuple3)
import Effect (Effect)
import JoeOp.Calendar.Day (Day)
import JoeOp.Calendar.Month (Month)
import JoeOp.Calendar.Year (Year)

foreign import _monthStartsOnWeekDay :: Int -> Int -> Int

foreign import _today :: (Int -> Int -> Int -> UnwrappedDate) -> Effect UnwrappedDate

type Date = Tuple3 Year Month Day

type UnwrappedDate = { year :: Int, month :: Int, day :: Int }

--| Calculate day of week, 0-6 = Sun. through Sat.,
--| from year and month
monthStartsOnWeekDay :: Year -> Int -> Int
monthStartsOnWeekDay yr mo = _monthStartsOnWeekDay (unwrap yr) mo

today :: Effect UnwrappedDate
today = _today (\year month day -> { year, month, day })

