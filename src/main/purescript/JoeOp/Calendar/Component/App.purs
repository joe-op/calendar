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

_month = Proxy :: Proxy "month"

component ::
  forall m.
  MonadAff m =>
  Array Date ->
  H.Component Query Input Output m
component dates =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  render _ =
    HH.main_
      ( dates <#>
          ( \date ->
              HH.div
                [ HP.classes [ HH.ClassName "cal-month-section" ] ]
                [ HH.slot_
                    _month
                    "1"
                    Calendar.component
                    ( Tuple
                        (Tuple.Nested.get1 date)
                        (Tuple.Nested.get2 date)
                    )
                ]
          )
      )
