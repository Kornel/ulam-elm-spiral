import Html exposing (Html)
import Window
import Svg exposing (text_, text, rect, svg)
import Svg.Attributes exposing (..)
import List exposing (length, concat, map, repeat, range, take, tail, map3)
import Task
-- import Action exposing (Msg(WindowResize, NoOp), update)

type Msg = Resize Int Int



zip3 : List a -> List b -> List c -> List (a, b, c)
zip3 = map3 (\a -> \b -> \c -> (a, b, c))

n = 20
elements = range 1 (n^2)

dx : Int -> Int
dx n = (-1)^((n+1) % 2)
dy : Int -> Int
dy n = (-1)^(n % 2)

dxs = elements
      |> map (\x -> repeat x (dx x) ++ repeat x 0)
      |> concat
      |> take (n^2 - 1)

dys = elements
      |> map (\x -> repeat x 0 ++ (repeat x (dy x)))
      |> concat
      |> take (n^2 - 1)


xs ww = dxs
      |> List.scanl (+) 0
      |> map (\x -> x * 30 + ww // 2)

ys wh = dys
      |> List.scanl (+) 0
      |> map (\x -> x * 30 + wh // 2)


coordAndNumbers ww wh = zip3 (xs ww) (ys wh) (range 1 (n^2))


type alias Model =
    { screen :
        { width : Int
        , height : Int
        },
      elements: List (Int, Int, Int)
    }

plotSpiral : Int -> Int -> List (Int, Int, Int) -> Html.Html msg
plotSpiral sw sh es =
  let
    toElem (a, b, c) =
      text_
        [ x (toString (a))
        , y (toString (sh - (b)))
        ] [Html.text (toString c)]
  in
    List.map toElem es
     |> svg [ width (toString sw), height (toString sh)]

sizeToMsg : Window.Size -> Msg
sizeToMsg size =
  Resize size.width size.height

initialSizeCmd : Cmd Msg
initialSizeCmd =
  Task.perform sizeToMsg Window.size

init : (Model, Cmd Msg)
init =
    ( { screen =
        {  width = 1024
         , height = 768
        },
        elements = coordAndNumbers 1024 768
      },
      initialSizeCmd
    )

view : Model -> Html Msg
view model =
    let
        sw = model.screen.width
        sh = model.screen.height
        es = model.elements
    in
      plotSpiral sw sh es

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let m =
        case msg of
            Resize w h -> {model | screen = {width = w, height = h}, elements = coordAndNumbers w h}
    in
        (m, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes (\size -> Resize size.width size.height)

main =
   Html.program
     { init = init
     , view = view
     , update = update
     , subscriptions = subscriptions
     }
