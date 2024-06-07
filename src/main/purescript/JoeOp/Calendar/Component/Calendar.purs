module JoeOp.Calendar.Component.Calendar
  ( Output
  , Query
  , Slot
  , component
  ) where

import Prelude
import Data.Const (Const)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import JoeOp.Calendar.Component.Calendar.Month as Month
import JoeOp.Calendar.Types (Month, Year)
import Type.Proxy (Proxy(..))

type Input = Tuple Year Month

type Output = Void

type Query :: forall k. k -> Type
type Query = Const Void

type Slot = H.Slot Query Output

type ChildSlots =
  ( month :: Month.Slot Unit
  )

type State =
  { selectedMonth ::
      { month :: Month
      , year :: Year
      }
  }

data Action = Init

type HalogenM m = H.HalogenM State Action ChildSlots Output m

type HTML m = H.ComponentHTML Action ChildSlots m

_month = Proxy :: Proxy "month"

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
      [ HH.h1_ [ HH.text "Calendar" ]
      , HH.slot_
          _month
          unit
          Month.component
          state.selectedMonth
      ]

  handleAction :: Action -> HalogenM m Unit
  handleAction = case _ of
    Init -> pure unit
