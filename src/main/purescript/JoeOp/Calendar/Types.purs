module JoeOp.Calendar.Types where

import Prelude
import Data.Newtype (class Newtype)

newtype Day = Day Int

derive instance eqDay :: Eq Day

derive instance newtypeDay :: Newtype Day _

data WeekRowDay
  = DayOfWeek Day
  | EmptyDay

type WeekRow =
  { sunday :: WeekRowDay
  , monday :: WeekRowDay
  , tuesday :: WeekRowDay
  , wednesday :: WeekRowDay
  , thursday :: WeekRowDay
  , friday :: WeekRowDay
  , saturday :: WeekRowDay
  }

newtype Year = Year Int

derive instance eqYear :: Eq Year

derive instance newtypeYear :: Newtype Year _
