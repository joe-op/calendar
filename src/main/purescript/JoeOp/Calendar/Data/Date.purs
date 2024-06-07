module JoeOp.Calendar.Data.Date where

import Prelude
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested (tuple3)
import Effect (Effect)
import JoeOp.Calendar.Data.Month as Month
import JoeOp.Calendar.Date as Date
import JoeOp.Calendar.Types (Date, Day(..), Month(..), Year(..))

unsafeToday :: Effect Date
unsafeToday =
  Date.today <#>
    ( \today ->
        tuple3 (Year today.year) (fromMaybe January (Month.fromCardinalInt today.month)) (Day today.day)

    )
