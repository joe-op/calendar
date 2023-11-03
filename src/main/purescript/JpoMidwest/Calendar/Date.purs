module JpoMidwest.Calendar.Date
  ( monthStartsOnWeekDay
  ) where

import Data.Newtype (unwrap)
import JpoMidwest.Calendar.Types (Year)

foreign import _monthStartsOnWeekDay :: Int -> Int -> Int

--| Calculate day of week, 0-6 = Sun. through Sat.,
--| from year and month
monthStartsOnWeekDay :: Year -> Int -> Int
monthStartsOnWeekDay yr mo = _monthStartsOnWeekDay (unwrap yr) mo
