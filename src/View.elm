module View exposing(view, initialSizeCmd, subscriptions)

import Window
import Task
import Html exposing (Html)
import UlamSvg exposing (plotSpiral)
import Model exposing(Msg, Model)


subscriptions : Model.Model -> Sub Model.Msg
subscriptions model = Window.resizes (\size -> Model.Resize size.width size.height)


sizeToMsg : Window.Size -> Model.Msg
sizeToMsg size = Model.Resize size.width size.height

initialSizeCmd : Cmd Model.Msg
initialSizeCmd = Task.perform sizeToMsg Window.size


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
