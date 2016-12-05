module Ulam exposing(computeXcoord, computeYcoord)
import List exposing (length, concat, map, repeat, range, take, tail, map3)
import Lazy.List

n = 100
wx = 10
wy = 10
elements = Lazy.List.iterate (\x -> x + 1) 1

xTerm n = (-1)^((n+1) % 2)
yTerm n = (-1)^(n % 2)

dxs = elements
      |> Lazy.List.map (\x -> Lazy.List.append (Lazy.List.fromList (List.repeat x (xTerm x))) (Lazy.List.fromList (List.repeat x 0)))
      |> Lazy.List.flatten
      |> Lazy.List.take (n^2 - 1)
      |> Lazy.List.toList

dys = elements
      |> Lazy.List.map (\x -> Lazy.List.append (Lazy.List.fromList (repeat x 0)) (Lazy.List.fromList (repeat x (yTerm x))))
      |> Lazy.List.flatten
      |> Lazy.List.take (n^2 - 1)
      |> Lazy.List.toList

computeXcoord shiftx = dxs
      |> List.scanl (+) 0
      |> map (\x -> x * wx + shiftx)

computeYcoord shifty = dys
      |> List.scanl (+) 0
      |> map (\y -> y * wy + shifty)
