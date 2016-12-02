import Html
import Svg exposing (text_, text, rect, svg)
import Svg.Attributes exposing (..)
import List exposing (length, concat, map, repeat, range, take, tail, map3)

zip3 : List a -> List b -> List c -> List (a, b, c)
zip3 = map3 (\a -> \b -> \c -> (a, b, c))

n = 10
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


xs = dxs
      |> List.scanl (+) 0
      |> map (\x -> x * 20 + 300)

ys = dys
      |> List.scanl (+) 0
      |> map (\x -> x * 20 + 300)


coordAndNumbers = zip3 xs ys (range 1 (n^2))

plotSpiral : List (Int, Int, Int) -> Html.Html msg
plotSpiral =
  let
    toElem (a, b, c) =
      text_
        [ x (toString (a))
        , y (toString (600 - (b)))
        ] [Html.text (toString c)]
  in
    svg [ width "600", height "600" ] << List.map toElem

main = plotSpiral <| coordAndNumbers
