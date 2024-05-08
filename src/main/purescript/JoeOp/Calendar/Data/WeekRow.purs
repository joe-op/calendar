module JoeOp.Calendar.Data.WeekRow where

import Control.Alternative ((<|>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import JoeOp.Calendar.Types (WeekRow, WeekRowDay(..))

weekRowDayInt :: WeekRowDay -> Maybe Int
weekRowDayInt = case _ of
  EmptyDay -> Nothing
  DayOfWeek day -> Just (unwrap day)

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
