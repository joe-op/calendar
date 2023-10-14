module JpoMidwest.Calendar.Component.Calendar
  ( component
  ) where

import Prelude
import Data.Const (Const)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

type Input
  = Unit

type Output
  = Void

type Query :: forall k. k -> Type
type Query
  = Const Void

type State
  = Unit

type Action
  = Void

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
              { handleAction = absurd
              }
    }
  where
  initialState _ = unit

  render _ = HH.h1_ [ HH.text "Calendar" ]
