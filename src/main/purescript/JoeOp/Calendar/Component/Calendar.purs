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
import Halogen as H
import Halogen.HTML as HH
import JoeOp.Calendar (Month, Year)
import JoeOp.Calendar.Component.Calendar.Controls as Controls
import JoeOp.Calendar.Component.Calendar.Month (Slot, component) as Month
import JoeOp.Calendar.Data.Date as Data.Date
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
    HH.div_
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
    where
    addMonths :: Int -> HalogenM m Unit
    addMonths n = do
      { month, year } <- H.gets _.selectedMonth
      let
        newYearMonth = Data.Date.addMonths year month n
      H.modify_ \s -> s { selectedMonth = { month: snd newYearMonth, year: fst newYearMonth } }

