module JoeOp.Calendar.Component.Calendar
  ( Output
  , Query
  , Slot
  , component
  ) where

import Prelude
import Data.Const (Const)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HH.Events
import Halogen.HTML.Properties as HP
import JoeOp.Calendar (Month, Year)
import JoeOp.Calendar.Component.Calendar.Controls as Controls
import JoeOp.Calendar.Component.Calendar.Month (Slot, component) as Month
import JoeOp.Calendar.Data.Date as Data.Date
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Type.Proxy (Proxy(..))

type Input = Tuple Year Month

type Output = Void

type Query :: forall k. k -> Type
type Query = Const Void

type Slot = H.Slot Query Output

type ChildSlots =
  ( controls :: Controls.Slot Unit
  , month :: Month.Slot Unit
  )

type State =
  { selectedMonth ::
      { month :: Month
      , year :: Year
      }
  }

data Action
  = Init
  | HandleControls Controls.Output
  | HandleKey KeyboardEvent

type HalogenM m = H.HalogenM State Action ChildSlots Output m

type HTML m = H.ComponentHTML Action ChildSlots m

_controls = Proxy :: Proxy "controls"
_month = Proxy :: Proxy "month"

--| Calendar component
--| may want to split into "Calendar" and "Calendar plus controls"
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
              }
    }
  where
  initialState (Tuple year month) =
    { selectedMonth: { year, month }
    }

  render :: State -> HTML m
  render state =
    HH.div
      [ HH.Events.onKeyUp HandleKey
      , HP.classes [ HH.ClassName "calendar" ]
      , HP.tabIndex (-1)
      ]
      [ HH.slot_
          _month
          unit
          Month.component
          state.selectedMonth
      , HH.slot
          _controls
          unit
          Controls.component
          unit
          HandleControls
      ]

  handleAction :: Action -> HalogenM m Unit
  handleAction = case _ of
    Init -> pure unit
    HandleControls controlsOutput -> case controlsOutput of
      Controls.PreviousMonth -> addMonths (-1)
      Controls.NextMonth -> addMonths 1
    HandleKey ke -> do
      H.liftEffect $ Console.info ("Handling key " <> KeyboardEvent.key ke)
      if isLeftKey ke then
        preventKeyEventDefault ke *> addMonths (-1)
      else if isRightKey ke then
        preventKeyEventDefault ke *> addMonths 1
      else
        pure unit
    where
    addMonths :: Int -> HalogenM m Unit
    addMonths n = do
      { month, year } <- H.gets _.selectedMonth
      let
        newYearMonth = Data.Date.addMonths year month n
      H.modify_ \s -> s { selectedMonth = { month: snd newYearMonth, year: fst newYearMonth } }

  preventKeyEventDefault :: KeyboardEvent -> HalogenM m Unit
  preventKeyEventDefault = KeyboardEvent.toEvent >>> Event.preventDefault >>> H.liftEffect

  isLeftKey :: KeyboardEvent -> Boolean
  isLeftKey ke = KeyboardEvent.key ke == "ArrowLeft"

  isRightKey :: KeyboardEvent -> Boolean
  isRightKey ke = KeyboardEvent.key ke == "ArrowRight"

