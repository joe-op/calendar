module JpoMidwest.Calendar.Component.Calendar.Month
  ( Output
  , Query
  , Slot
  , component
  ) where

import Prelude
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Newtype as Newtype
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import JpoMidwest.Calendar.Component.Calendar.Day as Day
import JpoMidwest.Calendar.Data.Month (Month)
import JpoMidwest.Calendar.Data.Month as Month
import JpoMidwest.Calendar.Types (Day(..), Year)
import Type.Proxy (Proxy(..))

type Slot
  = H.Slot Query Output

type Input
  = { month :: Month
    , year :: Year
    }

-- TODO: determine setup based on month and year
type Output
  = Void

type Query :: forall k. k -> Type
type Query
  = Const Void

type ChildSlots
  = ( day :: Day.Slot Int
    )

type State
  = { days :: Array (Array Day)
    , input :: Input
    }

data Action
  = Receive Input

type HalogenM m
  = H.HalogenM State Action ChildSlots Output m

type HTML m
  = H.ComponentHTML Action ChildSlots m

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
    { days
    , input
    }
    where
    days = setUpDays input.year input.month

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
          (map weekHtml state.days)
      ]
    where
    weekHtml :: Array Day -> HTML m
    weekHtml days =
      HH.div
        -- TODO: CSS framework
        [ HP.classes [ HH.ClassName "month-container", HH.ClassName "month-container__week" ]
        ]
        (map dayHtml days)

    dayHtml :: Day -> HTML m
    dayHtml day =
      HH.div
        -- TODO: CSS framework
        [ HP.classes [ HH.ClassName "month-container", HH.ClassName "month-container__day" ]
        ]
        [ HH.slot_ _day (unwrap day) Day.component day
        ]

  -- TODO: day of week labels
  handleAction :: Action -> HalogenM m Unit
  handleAction = case _ of
    Receive input -> unlessM (eq input <$> H.gets _.input) (H.modify_ _ { input = input })

  -- TODO: weekday labels
  -- TODO: offset for week
  -- TODO: change to Maybe Int and render an empty square for nothing
  -- TODO: pad last week
  setUpDays :: Year -> Month -> Array (Array Day)
  setUpDays year month =
    let
      numberOfDays = Month.numberOfDays year month

      nextDay :: Day -> Array (Array Day) -> Array (Array Day)
      nextDay day currentDays =
        if (unwrap day) > numberOfDays then
          currentDays
        else
          nextDay (Newtype.over Day ((+) 1) day) nextLevel
        where
        nextLevel = case Array.unsnoc currentDays of
          Just { init: xs, last: x } ->
            if Array.length x >= 7 then
              xs <> [ x, [ day ] ]
            else
              xs <> [ Array.snoc x day ]
          Nothing -> [ [ day ] ]
    in
      nextDay (Day 1) [ [] ]
