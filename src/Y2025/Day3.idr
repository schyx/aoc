module Y2025.Day3

import Data.Vect
import Data.SortedSet
import Data.String

%default total

getJoltages : String -> List Int
getJoltages line = map (\c => (the Int $ cast c) - 48) $ unpack line

append : Vect n a -> a -> Vect (S n) a
append [] y        = [y]
append (x :: xs) y = x :: append xs y

step : Bool -> Vect n Int -> Int -> Vect n Int
step _     []             _ = []
step taken (x :: [])      b =
  if      taken then [b]
  else if b > x then [b]
  else               [x]
step taken (x :: y :: xs) b =
  if      x < 0 then append (y :: xs) b
  else if taken then y :: step True (y :: xs) b
  else if y > x then y :: step True (y :: xs) b
  else               x :: step False (y :: xs) b

toInt : Int -> Vect n Int -> Int
toInt buildup []        = buildup
toInt buildup (x :: xs) = toInt (10 * buildup + x) xs

getJoltageLine1 : String -> Int
getJoltageLine1 line = toInt 0 $ foldl (step False) (replicate 2 (-1)) (getJoltages line)

export
solve2025D3P1 : String -> Int
solve2025D3P1 input = foldr (\line, prev => getJoltageLine1 line + prev) 0 $ lines $ trim input

getJoltageLine2 : String -> Int
getJoltageLine2 line = toInt 0 $ foldl (step False) (replicate 12 (-1)) (getJoltages line)

export
solve2025D3P2 : String -> Int
solve2025D3P2 input = foldr (\line, prev => getJoltageLine2 line + prev) 0 $ lines $ trim input
