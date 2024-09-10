module JoeOp.Calendar.Main where

import Prelude
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import JoeOp.Calendar.Component.App as App
import JoeOp.Calendar.Data.Date as Date

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    today <- H.liftEffect Date.unsafeToday
    nextMonth <- H.liftEffect (Date.unsafeTodayPlusMonths 1)
    runUI App.component [ today, nextMonth ] body
