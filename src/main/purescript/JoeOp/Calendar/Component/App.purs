module JoeOp.Calendar.Component.App where

import Prelude
import Data.Const (Const)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested as Tuple.Nested
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import JoeOp.Calendar (Date)
import JoeOp.Calendar.Component.Calendar as Calendar
import Type.Proxy (Proxy(..))

type Input = Unit

type Output = Void

type Query :: forall k. k -> Type
type Query = Const Void

type Slot = H.Slot Query Output

type ChildSlots =
  ( calendar :: Calendar.Slot String
  )

data Action = HandleCalendarOutput { focus :: Boolean, slotLabel :: String, output :: Calendar.Output }

type HalogenM m = H.HalogenM Unit Action ChildSlots Output m

_calendar = Proxy :: Proxy "calendar"

component ::
  forall m.
  MonadAff m =>
  Array Date ->
  H.Component Query Input Output m
component dates =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $
        H.defaultEval
          { handleAction = handleAction
          }
    }
  where
  render _ =
    HH.main_
      ( dates <#>
          ( \date ->
              HH.div
                [ HP.classes [ HH.ClassName "cal-month-section" ] ]
                [ HH.slot
                    _calendar
                    mainCalendarLabel
                    Calendar.component
                    ( Tuple
                        (Tuple.Nested.get1 date)
                        (Tuple.Nested.get2 date)
                    )
                    ( \output ->
                        HandleCalendarOutput { focus: true, slotLabel: mainCalendarLabel, output }
                    )
                ]
          )
      )
  mainCalendarLabel = "1"

  handleAction :: Action -> HalogenM m Unit
  handleAction = case _ of
    HandleCalendarOutput { focus, slotLabel, output } -> case output of
      Calendar.Initialized ->
        if focus then
          H.tell _calendar slotLabel Calendar.Focus
        else
          pure unit
