module JpoMidwest.Calendar.Types where

import Prelude
import Data.Newtype (class Newtype)

newtype Day
  = Day Int

derive instance eqDay :: Eq Day

derive instance newtypeDay :: Newtype Day _

newtype Year
  = Year Int

derive instance eqYear :: Eq Year

derive instance newtypeYear :: Newtype Year _
