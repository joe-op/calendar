module JoeOp.Calendar.Component.Calendar.Month
  ( Output
  , Query
  , Slot
  , component
  ) where

import Prelude
import Data.Array as Array
import Data.Array ((..))
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import JoeOp.Calendar.Component.Calendar.Day as Day
import JoeOp.Calendar.Data.Month (Month)
import JoeOp.Calendar.Data.Month as Month
import JoeOp.Calendar.Data.WeekRow as Data.WeekRow
import JoeOp.Calendar.Date as Date
import JoeOp.Calendar.Types (Day(..), WeekRow, WeekRowDay(..), Year)
import Type.Proxy (Proxy(..))

type Slot = H.Slot Query Output

type Input =
  { month :: Month
  , year :: Year
  }

-- TODO: determine setup based on month and year
type Output = Void

type Query :: forall k. k -> Type
type Query = Const Void

type ChildSlots =
  ( day :: Day.Slot Int
  )

type State =
  { input :: Input
  , weekRows :: Array WeekRow
  }

data Action = Receive Input

type HalogenM m = H.HalogenM State Action ChildSlots Output m

type HTML m = H.ComponentHTML Action ChildSlots m

_day = Proxy :: Proxy "day"

component ::
  forall m.
  MonadAff m =>
  H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = Just <<< Receive
              }
    }
  where
  initialState :: Input -> State
  initialState input =
    { input
    , weekRows: setUpDays input.year input.month
    }

  render :: State -> HTML m
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text
              ( Month.toString state.input.month
                  <> " "
                  <> (show $ unwrap state.input.year)
              )
          ]
      , HH.div
          -- TODO: CSS framework
          [ HP.classes [ HH.ClassName "month-container" ]
          ]
          ( append
              [ weekDayLabels ]
              (map weekHtml state.weekRows)
          )
      ]
    where
    weekHtml :: WeekRow -> HTML m
    weekHtml weekRow =
      HH.div
        -- TODO: CSS framework
        [ HP.classes [ HH.ClassName "month-container", HH.ClassName "month-container__week" ]
        ]
        [ dayHtml weekRow.sunday
        , dayHtml weekRow.monday
        , dayHtml weekRow.tuesday
        , dayHtml weekRow.wednesday
        , dayHtml weekRow.thursday
        , dayHtml weekRow.friday
        , dayHtml weekRow.saturday
        ]

    dayHtml :: WeekRowDay -> HTML m
    dayHtml day =
      HH.div
        -- TODO: CSS framework
        [ HP.classes [ HH.ClassName "month-container", HH.ClassName "month-container__day" ]
        ]
        [ case day of
            DayOfWeek d -> HH.slot_ _day (unwrap d) Day.component d
            EmptyDay -> Day.emptyDay
        ]

  -- TODO: day of week labels
  handleAction :: Action -> HalogenM m Unit
  handleAction = case _ of
    Receive input -> unlessM (eq input <$> H.gets _.input) (H.modify_ _ { input = input })

  -- TODO: offset for week
  -- TODO: change to Maybe Int and render an empty square for nothing
  -- TODO: pad last week
  setUpDays :: Year -> Month -> Array WeekRow
  setUpDays year month =
    let
      startingWeekDay = Date.monthStartsOnWeekDay year (Month.toCardinalInt month)

      numberOfDays = Month.numberOfDays year month

      weekRowFromArray :: Array WeekRowDay -> WeekRow
      weekRowFromArray weekRowDays =
        let
          elemOrEmpty :: Int -> WeekRowDay
          elemOrEmpty i = fromMaybe EmptyDay (Array.index weekRowDays i)
        in
          { sunday: elemOrEmpty 0
          , monday: elemOrEmpty 1
          , tuesday: elemOrEmpty 2
          , wednesday: elemOrEmpty 3
          , thursday: elemOrEmpty 4
          , friday: elemOrEmpty 5
          , saturday: elemOrEmpty 6
          }

      startingWeek :: WeekRow
      startingWeek =
        let
          buildArr :: Array WeekRowDay -> Array WeekRowDay
          buildArr arr =
            if Array.length arr >= 7 then
              arr
            else if Array.length arr < startingWeekDay then
              buildArr (Array.snoc arr EmptyDay)
            else
              let
                startingDay = 1
                endingDay = 7 - (Array.length arr)
              in
                append arr ((startingDay .. endingDay) <#> (Day >>> DayOfWeek))
        in
          weekRowFromArray (buildArr [])

      nextWeek :: Int -> WeekRow
      nextWeek highestDayAchieved =
        let
          wrd :: Int -> WeekRowDay
          wrd i =
            if i > numberOfDays then
              EmptyDay
            else
              DayOfWeek (Day i)
        in
          { sunday: wrd $ highestDayAchieved + 1
          , monday: wrd $ highestDayAchieved + 2
          , tuesday: wrd $ highestDayAchieved + 3
          , wednesday: wrd $ highestDayAchieved + 4
          , thursday: wrd $ highestDayAchieved + 5
          , friday: wrd $ highestDayAchieved + 6
          , saturday: wrd $ highestDayAchieved + 7
          }

      buildWeeks :: Array WeekRow -> Array WeekRow
      buildWeeks = case _ of
        [] -> buildWeeks [ startingWeek ]
        wkrs ->
          if highestDayAchieved wkrs >= numberOfDays then
            wkrs
          else if Array.length wkrs >= 5 then
            -- TODO: fix
            wkrs
          else
            buildWeeks (Array.snoc wkrs (nextWeek $ highestDayAchieved wkrs))

        where
        highestDayAchieved :: Array WeekRow -> Int
        highestDayAchieved wkrs =
          maybe 0 Data.WeekRow.highestWeekRowDay (Array.last wkrs)

    in
      buildWeeks []

  weekDayLabels :: HTML m
  weekDayLabels =
    HH.div
      [ HP.classes [ HH.ClassName "month-container", HH.ClassName "month-container__weekday-labels" ]
      ]
      ( map
          ( \d -> HH.div
              [ HP.classes [ HH.ClassName "month-container", HH.ClassName "month-container__weekday-label" ]
              ]
              [ HH.text d
              ]
          )
          weekDays
      )

  weekDays =
    [ "Sunday"
    , "Monday"
    , "Tuesday"
    , "Wednesday"
    , "Thursday"
    , "Friday"
    , "Saturday"
    ]
