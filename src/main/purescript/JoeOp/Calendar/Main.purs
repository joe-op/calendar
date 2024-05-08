module JoeOp.Calendar.Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import JoeOp.Calendar.Component.App as App

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI App.component unit body
