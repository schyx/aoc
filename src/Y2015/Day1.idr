module Y2015.Day1

import Data.String

export
solve2015D1P1 : String -> Int
solve2015D1P1 input = go 0 $ unpack $ trim input
  where
    go : Int -> List Char -> Int
    go floor []            = floor
    go floor ('(' :: rest) = go (floor + 1) rest
    go floor (')' :: rest) = go (floor - 1) rest
    go floor _             = 0

export
solve2015D1P2 : String -> Int
solve2015D1P2 input = go 0 1 $ unpack $ trim input
  where
    go : Int -> Int -> List Char -> Int
    go floor count []            = 0
    go floor count ('(' :: rest) = go (floor + 1) (count + 1) rest
    go floor count (')' :: rest) = if floor == 0 then count else go (floor - 1) (count + 1) rest
    go floor count _             = 0
