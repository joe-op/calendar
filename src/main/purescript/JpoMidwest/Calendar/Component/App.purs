module JpoMidwest.Calendar.Component.App where

import Prelude
import Data.Const (Const)
import DevReload.Component as DevReload
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import JpoMidwest.Calendar.Component.Calendar as Calendar
import Type.Proxy (Proxy(..))

type Input
  = Unit

type Output
  = Void

type Query :: forall k. k -> Type
type Query
  = Const Void

type Slot
  = H.Slot Query Output

type ChildSlots
  = ( calendar :: Calendar.Slot Unit
    , devtools :: DevReload.Slot Unit
    )

_devtools = Proxy :: Proxy "devtools"

_month = Proxy :: Proxy "month"

component ::
  forall m.
  MonadAff m =>
  H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  render _ =
    HH.div_
      [ HH.slot_
          _devtools
          unit
          DevReload.component
          unit
      , HH.slot_
          _month
          unit
          Calendar.component
          unit
      ]
