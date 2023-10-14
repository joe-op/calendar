module JpoMidwest.Calendar.Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import JpoMidwest.Calendar.Component.Calendar as Calendar

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI Calendar.component unit body
