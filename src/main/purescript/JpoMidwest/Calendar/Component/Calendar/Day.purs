module JpoMidwest.Calendar.Component.Calendar.Day
  ( Output
  , Query
  , Slot
  , component
  , emptyDay
  ) where

import Prelude
import Data.Const (Const)
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import JpoMidwest.Calendar.Types (Day)

type Slot = H.Slot Query Output

type Input = Day

type Output = Void

type Query :: forall k. k -> Type
type Query = Const Void

type State =
  { input :: Input
  }

type Action = Void

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
  initialState = { input: _ }

  render state =
    HH.div
      -- TODO: CSS framework
      [ HP.classes [ HH.ClassName "day-container" ]
      ]
      [ HH.div
          [ HP.classes [ HH.ClassName "day-container", HH.ClassName "day-container__number" ]
          ]
          [ HH.text $ show $ unwrap state.input
          ]
      ]

emptyDay :: forall action props. HH.HTML props action
emptyDay =
  HH.div
    [ HP.classes [ HH.ClassName "day-container" ]
    ]
    []
