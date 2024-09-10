module JoeOp.Calendar.Data.Date where

import Prelude
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested (tuple3)
import Effect (Effect)
import JoeOp.Calendar.Data.Month as Month
import JoeOp.Calendar.Date as Date
import JoeOp.Calendar.Types (Date, Day(..), Month(..), UnwrappedDate, Year(..))

wrappedDate :: UnwrappedDate -> Date
wrappedDate { year, month, day } = tuple3 (Year year) (fromMaybe January (Month.fromCardinalInt month)) (Day day)

unsafeTodayPlusMonths :: Int -> Effect Date
unsafeTodayPlusMonths months =
  Date.today <#>
    ( \today ->
        wrappedDate (today { month = mod (today.month + months) 12 })
    )

unsafeToday :: Effect Date
unsafeToday = Date.today <#> wrappedDate
