module JoeOp.Calendar.Month
  ( Month(..)
  , addMod
  , fromCardinalInt
  , numberOfDays
  , toCardinalInt
  , toIndex
  , toString
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import JoeOp.Calendar.Year (Year)

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

addMod :: Int -> Month -> Month
addMod n month = fromIntMod (n + (toInt month))

toIndex :: Month -> Int
toIndex = toInt

--| 0-indexed toInt
toInt :: Month -> Int
toInt = case _ of
  January -> 0
  February -> 1
  March -> 2
  April -> 3
  May -> 4
  June -> 5
  July -> 6
  August -> 7
  September -> 8
  October -> 9
  November -> 10
  December -> 11

fromIntMod :: Int -> Month
fromIntMod n = case n of
  0 -> January
  1 -> February
  2 -> March
  3 -> April
  4 -> May
  5 -> June
  6 -> July
  7 -> August
  8 -> September
  9 -> October
  10 -> November
  11 -> December
  _ -> fromIntMod (mod n 12)

fromCardinalInt :: Int -> Maybe Month
fromCardinalInt = case _ of
  i
    | i == 1 -> Just January
    | i == 2 -> Just February
    | i == 3 -> Just March
    | i == 4 -> Just April
    | i == 5 -> Just May
    | i == 6 -> Just June
    | i == 7 -> Just July
    | i == 8 -> Just August
    | i == 9 -> Just September
    | i == 10 -> Just October
    | i == 11 -> Just November
    | i == 12 -> Just December
    | otherwise -> Nothing

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
