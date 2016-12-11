module Update exposing(update)

import Model exposing(..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let m =
      case msg of
        Resize w h ->
          {model | screen = {width = w, height = h} }
        Move dx dy ->
          {model | screen = {width = model.screen.width + dx, height = model.screen.height + dy} }
    in
      (m, Cmd.none)
