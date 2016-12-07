import Html exposing (Html)
import Window
import Svg exposing (text_, text, rect, svg, polyline)
import Svg.Attributes exposing (..)
import List exposing (length, concat, map, repeat, range, take, tail, map4, scanl)
import String exposing (join)
import Task
import Arithmetic exposing(isPrime)
import Ulam exposing(..)
import UlamSvg exposing (..)

type Msg = Resize Int Int

type alias Model =
    { screen :
        { width : Int
        , height : Int
        },
      spiral :
        {  n : Int
         , wx : Int
         , wy : Int
        }
    }

sizeToMsg : Window.Size -> Msg
sizeToMsg size = Resize size.width size.height

initialSizeCmd : Cmd Msg
initialSizeCmd = Task.perform sizeToMsg Window.size

init : (Model, Cmd Msg)
init =
    ( { screen =
        {  width = 0
         , height = 0
        },
        spiral =
        {  n = 50
         , wx = 12
         , wy = 12
        }
      },
      initialSizeCmd
    )

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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let m =
      case msg of
        Resize w h -> {model | screen = {width = w, height = h} }
    in
      (m, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model = Window.resizes (\size -> Resize size.width size.height)

main = Html.program
     { init = init
     , view = view
     , update = update
     , subscriptions = subscriptions
     }
