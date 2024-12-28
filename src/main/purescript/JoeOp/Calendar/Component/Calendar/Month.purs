module JoeOp.Calendar.Component.Calendar.Month
  ( Output
  , Query
  , Slot
  , component
  ) where

import Prelude
import Data.Array ((:), (..))
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import JoeOp.Calendar (Day(..), Month, WeekRow, WeekRowDay(..), Year)
import JoeOp.Calendar.Component.Calendar.Day as Day
import JoeOp.Calendar.Date as Date
import JoeOp.Calendar.Month as Month
import JoeOp.Calendar.Week as Week
import Type.Proxy (Proxy(..))

type Slot = H.Slot Query Output

type Input =
  { month :: Month
  , year :: Year
  }

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
          ( Array.concat
              [ [ weekDayLabels ]
              , map weekHtml state.weekRows
              , spacingWeekRows (Array.length state.weekRows)
              ]
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
    dayHtml = dayHtml' []

    dayHtml' :: Array String -> WeekRowDay -> HTML m
    dayHtml' modifiers day =
      HH.div
        -- TODO: CSS framework
        [ HP.classes
            ( map HH.ClassName
                ( Array.concat
                    [ [ "month-container"
                      , "month-container__day"
                      ]
                    , (map ((<>) "month-container__day--") modifiers)
                    ]
                )
            )
        ]
        [ case day of
            DayOfWeek d -> HH.slot_ _day (unwrap d) Day.component d
            EmptyDay -> Day.emptyDay
        ]

    --| Always take up 6 rows of space so that controls don't jump around.
    --| Putting the controls at the top would also fix this of course,
    --| but it's nicer to have them on the bottom on a mobile screen.
    spacingWeekRows :: Int -> Array (HTML m)
    spacingWeekRows = spacingRows []
      where
      spacingRows :: Array (HTML m) -> Int -> Array (HTML m)
      spacingRows result len =
        if len >= 6 then
          result
        else
          spacingRows
            (spacingRow : result)
            (len + 1)

      spacingRow :: HTML m
      spacingRow =
        HH.div
          [ HP.classes
              [ HH.ClassName "month-container"
              , HH.ClassName "month-container__week"
              , HH.ClassName "month-container__week--spacer"
              ]
          ]
          (Array.replicate 7 (dayHtml' [ "spacer" ] EmptyDay))

  handleAction :: Action -> HalogenM m Unit
  handleAction = case _ of
    Receive input -> unlessM
      (eq input <$> H.gets _.input)
      ( H.modify_ _
          { input = input
          , weekRows = setUpDays input.year input.month
          }
      )

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
          else
            buildWeeks (Array.snoc wkrs (nextWeek $ highestDayAchieved wkrs))

        where
        highestDayAchieved :: Array WeekRow -> Int
        highestDayAchieved wkrs =
          maybe 0 Week.highestWeekRowDay (Array.last wkrs)

    in
      buildWeeks []

  weekDayLabels :: HTML m
  weekDayLabels =
    HH.div
      [ HP.classes [ HH.ClassName "month-container", HH.ClassName "month-container__weekday-labels" ]
      ]
      ( map
          ( \(Tuple d dabbrev) ->
              HH.div_
                [ HH.span
                    [ HP.classes [ HH.ClassName "month-container", HH.ClassName "month-container__weekday-label" ]
                    ]
                    [ HH.text d
                    ]
                , HH.span
                    [ HP.classes [ HH.ClassName "month-container", HH.ClassName "month-container__weekday-label", HH.ClassName "month-container__weekday-label--mobile" ]
                    ]
                    [ HH.text dabbrev
                    ]
                ]
          )
          weekDays
      )

  weekDays =
    [ Tuple "Sunday" "Sun"
    , Tuple "Monday" "Mon"
    , Tuple "Tuesday" "Tue"
    , Tuple "Wednesday" "Wed"
    , Tuple "Thursday" "Thur"
    , Tuple "Friday" "Fri"
    , Tuple "Saturday" "Sat"
    ]
