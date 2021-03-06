module Ulam exposing(dxs, dys)

import List exposing (repeat, concat, range, map)

xTerm : Int -> Int
xTerm n =
  (-1)^((n+1) % 2)


yTerm : Int -> Int
yTerm n =
  (-1)^(n % 2)


dxs : Int -> List Int
dxs n =
  range 1 (4 * n)
      |> map (\x -> repeat x (xTerm x) ++ List.repeat x 0)
      |> concat

dys : Int -> List Int
dys n =
  range 1 (4 * n)
      |> map (\x -> List.repeat x 0 ++ repeat x (yTerm x))
      |> concat
