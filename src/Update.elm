module Update exposing(update)

import Model exposing(..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let m =
      case msg of
        Resize w h -> {model | screen = {width = w, height = h} }
    in
      (m, Cmd.none)
