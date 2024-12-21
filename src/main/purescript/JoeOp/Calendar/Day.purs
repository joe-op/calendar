module JoeOp.Calendar.Day
  ( Day(..)
  ) where

import Prelude
import Data.Newtype (class Newtype)

newtype Day = Day Int

derive instance eqDay :: Eq Day

derive instance newtypeDay :: Newtype Day _
