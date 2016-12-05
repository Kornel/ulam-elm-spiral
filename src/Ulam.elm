module Ulam exposing(dxs, dys)
import List exposing (repeat)
import Lazy.List exposing (..)

elements = iterate (\x -> x + 1) 1

xTerm n = (-1)^((n+1) % 2)
yTerm n = (-1)^(n % 2)

dxs n = elements
      |> Lazy.List.map (\x -> append (fromList <| List.repeat x (xTerm x)) (fromList <| List.repeat x 0))
      |> flatten
      |> take (n^2 - 1)
      |> toList

dys n = elements
      |> Lazy.List.map (\x -> append (fromList <| List.repeat x 0) (fromList <| List.repeat x (yTerm x)))
      |> flatten
      |> take (n^2 - 1)
      |> toList
