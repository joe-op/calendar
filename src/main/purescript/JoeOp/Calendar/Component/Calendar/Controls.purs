module JoeOp.Calendar.Component.Calendar.Controls
  ( Output(..)
  , Query
  , Slot
  , component
  ) where

import Prelude
import Data.Const (Const)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

type Slot = H.Slot Query Output

type Input = Unit

type Query :: forall k. k -> Type
type Query = Const Void

data Output
  = NextMonth
  | PreviousMonth

data Action
  = RaiseNext MouseEvent
  | RaisePrevious MouseEvent

type HalogenM m = H.HalogenM Unit Action () Output m

type HTML m = H.ComponentHTML Action () m

component ::
  forall m.
  MonadAff m =>
  H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              }
    }
  where
  render :: Unit -> HTML m
  render _ =
    HH.div
      [ HP.classes [ HH.ClassName "calendar-controls" ] ]
      [ HH.span_
          [ HH.a
              [ HE.onClick RaisePrevious
              , HP.classes [ HH.ClassName "calendar-controls__link" ]
              , HP.href "#"
              ]
              [ HH.text "Previous"
              ]

          , HH.span_
              [ HH.a
                  [ HE.onClick RaiseNext
                  , HP.classes [ HH.ClassName "calendar-controls__link" ]
                  , HP.href "#"
                  ]
                  [ HH.text "Next"
                  ]
              ]
          ]
      ]

  handleAction :: Action -> HalogenM m Unit
  handleAction = case _ of
    RaiseNext me -> do
      H.liftEffect $ preventDefault (ME.toEvent me)
      H.raise NextMonth
    RaisePrevious me -> do
      H.liftEffect $ preventDefault (ME.toEvent me)
      H.raise PreviousMonth

