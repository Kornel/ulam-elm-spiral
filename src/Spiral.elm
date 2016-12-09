import Html exposing (Html)

import Model
import View
import Update


main = Html.program
     { init = Model.init View.initialSizeCmd
     , view = View.view
     , update = Update.update
     , subscriptions = View.subscriptions
     }
