module JoeOp.Calendar.Week
  ( WeekRow
  , WeekRowDay(..)
  , highestWeekRowDay
  , weekRowDayInt
  ) where

import Control.Alternative ((<|>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import JoeOp.Calendar.Day (Day)

type WeekRow =
  { sunday :: WeekRowDay
  , monday :: WeekRowDay
  , tuesday :: WeekRowDay
  , wednesday :: WeekRowDay
  , thursday :: WeekRowDay
  , friday :: WeekRowDay
  , saturday :: WeekRowDay
  }

data WeekRowDay
  = DayOfWeek Day
  | EmptyDay

highestWeekRowDay :: WeekRow -> Int
highestWeekRowDay weekRow =
  fromMaybe 0
    ( weekRowDayInt weekRow.saturday
        <|> weekRowDayInt weekRow.friday
        <|> weekRowDayInt weekRow.thursday
        <|> weekRowDayInt weekRow.wednesday
        <|> weekRowDayInt weekRow.tuesday
        <|> weekRowDayInt weekRow.monday
        <|> weekRowDayInt weekRow.sunday
    )

weekRowDayInt :: WeekRowDay -> Maybe Int
weekRowDayInt = case _ of
  EmptyDay -> Nothing
  DayOfWeek day -> Just (unwrap day)
