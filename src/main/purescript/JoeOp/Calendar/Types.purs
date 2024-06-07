module JoeOp.Calendar.Types where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (Tuple3)

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

data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December

derive instance eqMonth :: Eq Month

derive instance genericMonth :: Generic Month _

instance showMonth :: Show Month where
  show = genericShow

newtype Year = Year Int

derive instance eqYear :: Eq Year

derive instance newtypeYear :: Newtype Year _

type Date = Tuple3 Year Month Day

type UnwrappedDate = { year :: Int, month :: Int, day :: Int }
