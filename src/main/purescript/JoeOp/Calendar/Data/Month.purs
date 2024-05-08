module JoeOp.Calendar.Data.Month
  ( Month(..)
  , numberOfDays
  , toCardinalInt
  , toString
  ) where

import Prelude
import Data.Newtype (unwrap)
import JoeOp.Calendar.Types (Year)

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

numberOfDays :: Year -> Month -> Int
numberOfDays year = case _ of
  January -> 31
  February -> if isLeapYear year then 29 else 28
  March -> 31
  April -> 30
  May -> 31
  June -> 30
  July -> 31
  August -> 31
  September -> 30
  October -> 31
  November -> 30
  December -> 31
  where
  -- TODO: more accurate leap year calculation
  -- TODO: date utils module (?)
  isLeapYear :: Year -> Boolean
  isLeapYear = unwrap >>> flip mod 4 >>> eq 0

toCardinalInt :: Month -> Int
toCardinalInt = case _ of
  January -> 1
  February -> 2
  March -> 3
  April -> 4
  May -> 5
  June -> 6
  July -> 7
  August -> 8
  September -> 9
  October -> 10
  November -> 11
  December -> 12

--| Display Month in long form
toString :: Month -> String
toString = case _ of
  January -> "January"
  February -> "February"
  March -> "March"
  April -> "April"
  May -> "May"
  June -> "June"
  July -> "July"
  August -> "August"
  September -> "September"
  October -> "October"
  November -> "November"
  December -> "December"
