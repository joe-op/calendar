module JoeOp.Calendar.Component.App where

import Prelude
import Data.Const (Const)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested as Tuple.Nested
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import JoeOp.Calendar.Component.Calendar as Calendar
import JoeOp.Calendar.Types (Date)
import Type.Proxy (Proxy(..))

type Input = Date

type Output = Void

type Query :: forall k. k -> Type
type Query = Const Void

type Slot = H.Slot Query Output

type ChildSlots =
  ( calendar :: Calendar.Slot Unit
  )

_month = Proxy :: Proxy "month"

component ::
  forall m.
  MonadAff m =>
  H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  render date =
    HH.div_
      [ HH.slot_
          _month
          unit
          Calendar.component
          ( Tuple
              (Tuple.Nested.get1 date)
              (Tuple.Nested.get2 date)
          )
      ]
