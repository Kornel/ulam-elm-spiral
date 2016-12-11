module View exposing(view, initialSizeCmd, subscriptions)

import Window
import Task
import Html exposing (Html)
import UlamSvg exposing (plotSpiral)
import Model exposing(Msg, Model)
import Keyboard exposing (KeyCode)

subscriptions : Model.Model -> Sub Model.Msg
subscriptions model =
     Sub.batch
        [ Window.resizes (\size -> Model.Resize size.width size.height)
        , Keyboard.presses key
        ]


key : KeyCode -> Model.Msg
key keycode =
  case keycode of
    37 ->
      Model.Move -10 0

    39 ->
      Model.Move 10 0

    40 ->
      Model.Move 0 -10

    38 ->
      Model.Move 0 10

    _ ->
      Model.Move 0 0


sizeToMsg : Window.Size -> Model.Msg
sizeToMsg size =
  Model.Resize size.width size.height

initialSizeCmd : Cmd Model.Msg
initialSizeCmd =
  Task.perform sizeToMsg Window.size


view : Model -> Html Msg
view model =
    let
      sw = model.screen.width
      sh = model.screen.height
      wx = model.spiral.wx
      wy = model.spiral.wy
      n = model.spiral.n
    in
      plotSpiral n wx wy sw sh
