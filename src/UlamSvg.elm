module UlamSvg exposing(plotSpiral)

import Html exposing (Html)
import Svg exposing (text_, svg, polyline)
import Svg.Attributes exposing (fill, stroke, points, x, y, fontSize, width, height)
import List exposing (map4, scanl, map, range, filter)
import String exposing (join)
import Arithmetic exposing(isPrime)
import Ulam exposing (dxs, dys)

type alias UElement = (Int, Int, Int, Bool)
type alias UElements = List UElement

zip4 : List a -> List b -> List c -> List d -> List (a, b, c, d)
zip4 = map4 (\a b c d -> (a, b, c, d))


computeShiftedCoords : Int -> Int -> List Int -> List Int
computeShiftedCoords slope intercept elems = elems
      |> scanl (+) 0
      |> map (\x -> x * slope + intercept)


computeXcoords : Int -> Int -> Int -> List Int
computeXcoords n elemWidth centreX =
  computeShiftedCoords elemWidth centreX (dxs n)


computeYcoords : Int -> Int -> Int -> List Int
computeYcoords n elemHeight centreY =
  computeShiftedCoords elemHeight centreY (dys n)


coordsAndNumbers : Int -> Int -> Int -> Int -> Int -> UElements
coordsAndNumbers n elemWidth elemHeight screenWidth screenHeight =
  let
    xs = computeXcoords n elemWidth (screenWidth // 2)
    ys = computeYcoords n elemHeight (screenHeight // 2)
    nums = range 1 (n^2)
    whichPrime = map isPrime nums
  in
    zip4 xs ys nums whichPrime


filterPrimes : UElements -> UElements
filterPrimes elems =
  filter (\(x, y, n, p) -> p) elems


polylineSvgCoord : UElement -> String
polylineSvgCoord (x, y, _, _) =
  (toString x) ++ "," ++ (toString y)


polylineSvgCoords : UElements -> String
polylineSvgCoords elems =
  join "," (map polylineSvgCoord elems)


plotSpiral : Int -> Int -> Int -> Int -> Int -> Html.Html msg
plotSpiral n wx wy screenWidth screenHeight =
  let
    strWidth = toString screenWidth
    strHeight = toString screenHeight

    elems = coordsAndNumbers n wx wy screenWidth screenHeight
    primes = filterPrimes elems

    toPoly elems =
      polyline
        [ fill "none"
        , stroke "black"
        , points <| polylineSvgCoords primes
        ] []

    toText (xcoord, ycoord, n, prime) =
      text_
        [ x (toString xcoord)
        , y (toString ycoord)
        , fontSize "8"
        ] [Html.text (if prime then (toString n) else "-")]
  in
    [toPoly elems] ++ (List.map toText elems)
     |> svg [ width strWidth, height strHeight]
