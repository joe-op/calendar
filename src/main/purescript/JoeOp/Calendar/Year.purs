module JoeOp.Calendar.Year (Year(..)) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype Year = Year Int

derive instance eqYear :: Eq Year

derive instance newtypeYear :: Newtype Year _

derive instance genericYear :: Generic Year _

instance showYear :: Show Year where
  show = genericShow
