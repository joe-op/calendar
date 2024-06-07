module JoeOp.Calendar.Date
  ( monthStartsOnWeekDay
  , today
  ) where

import Data.Newtype (unwrap)
import Effect (Effect)
import JoeOp.Calendar.Types (UnwrappedDate, Year)

foreign import _monthStartsOnWeekDay :: Int -> Int -> Int

foreign import _today :: (Int -> Int -> Int -> UnwrappedDate) -> Effect UnwrappedDate

--| Calculate day of week, 0-6 = Sun. through Sat.,
--| from year and month
monthStartsOnWeekDay :: Year -> Int -> Int
monthStartsOnWeekDay yr mo = _monthStartsOnWeekDay (unwrap yr) mo

today :: Effect UnwrappedDate
today = _today (\year month day -> { year, month, day })

