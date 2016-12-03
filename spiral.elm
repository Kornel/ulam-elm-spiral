import Html exposing (Html)
import Window
import Svg exposing (text_, text, rect, svg)
import Svg.Attributes exposing (..)
import List exposing (length, concat, map, repeat, range, take, tail, map3)
import Task

type Msg = Resize Int Int

zip3 : List a -> List b -> List c -> List (a, b, c)
zip3 = map3 (\a -> \b -> \c -> (a, b, c))

n = 30
elements = range 1 (n^2)

dx : Int -> Int
dx n = (-1)^((n+1) % 2)
dy : Int -> Int
dy n =  (-1)^(n % 2)

dxs = elements
      |> map (\x -> repeat x (dx x) ++ repeat x 0)
      |> concat
      |> take (n^2 - 1)

dys = elements
      |> map (\x -> repeat x 0 ++ (repeat x (dy x)))
      |> concat
      |> take (n^2 - 1)

computeXcoord shiftx = dxs
      |> List.scanl (+) 0
      |> map (\x -> x * 30 + shiftx)

computeYcoord shifty = dys
      |> List.scanl (+) 0
      |> map (\x -> x * 30 + shifty)

coordAndNumbers screenWidth screenHeight =
  let
    xs = computeXcoord (screenWidth // 2)
    ys = computeYcoord (screenHeight // 2)
    nums = range 1 (n^2)
  in
    zip3 xs ys nums

type alias UElements = List (Int, Int, Int)
type alias Model =
    { screen :
        { width : Int
        , height : Int
        },
      elements: UElements
    }

plotSpiral : Int -> Int -> UElements -> Html.Html msg
plotSpiral screenWidth screenHeight elements =
  let
    toText (xcoord, ycoord, n) =
      text_
        [ x (toString xcoord)
        , y (toString ycoord)
        ] [Html.text (toString n)]
    strWidth = toString screenWidth
    strHeight = toString screenHeight
  in
    List.map toText elements
     |> svg [ width strWidth, height strHeight]

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
        elements = []
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
        Resize w h -> {model | screen = {width = w, height = h}
                             , elements = coordAndNumbers w h}
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
