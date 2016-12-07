import Html exposing (Html)
import Window
import Svg exposing (text_, text, rect, svg, polyline)
import Svg.Attributes exposing (..)
import List exposing (length, concat, map, repeat, range, take, tail, map3, scanl)
import String exposing (join)
import Task
import Arithmetic exposing(isPrime)
import Ulam exposing(..)

type Msg = Resize Int Int

zip3 : List a -> List b -> List c -> List (a, b, c)
zip3 = map3 (\a -> \b -> \c -> (a, b, c))

computeShiftedCoords : Int -> Int -> List Int -> List Int
computeShiftedCoords slope intercept elems = elems
      |> scanl (+) 0
      |> map (\x -> x * slope + intercept)

computeXcoords n wx shiftx = computeShiftedCoords wx shiftx (dxs n)
computeYcoords n wy shifty = computeShiftedCoords wy shifty (dys n)

coordsAndNumbers : Int -> Int -> Int -> Int -> Int -> List (Int, Int, Int)
coordsAndNumbers n wx wy screenWidth screenHeight =
  let
    xs = computeXcoords n wx (screenWidth // 2)
    ys = computeYcoords n wy (screenHeight // 2)
    nums = range 1 (n^2)
  in
    zip3 xs ys nums

type alias UElements = List (Int, Int, Int)
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

plotSpiral : Int -> Int -> UElements -> Html.Html msg
plotSpiral screenWidth screenHeight elements =
  let
    primeElements elems = List.filter (\(x, y, n) -> isPrime n) elems
    toCoord (xcoord, ycoord, n) = (toString xcoord) ++ "," ++ (toString ycoord)
    toCoordString elems  = join "," (map toCoord elems )
    toPoly : UElements -> Svg.Svg msg
    toPoly elems = polyline [ fill "none", stroke "black", points (toCoordString (primeElements elems)) ] []
    toText (xcoord, ycoord, n) =
      text_
        [ x (toString xcoord)
        , y (toString ycoord)
        , fontSize "8"
        ] [Html.text (if (isPrime n) then (toString n) else "")]
    strWidth = toString screenWidth
    strHeight = toString screenHeight

  in
    [toPoly elements] -- ++ (List.map toText elements)
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
        spiral =
        {  n = 50
         , wx = 10
         , wy = 10
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
      es = coordsAndNumbers n wx wy sw sh
    in
      plotSpiral sw sh es

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
