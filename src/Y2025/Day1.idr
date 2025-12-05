module Y2025.Day1

import Data.String

%default total

data Turn : Type where
  L : Int -> Turn
  R : Int -> Turn

toTurn : String -> Maybe Turn
toTurn input = case unpack input of
  ('L' :: rest) => Just $ L $ cast $ pack rest
  ('R' :: rest) => Just $ R $ cast $ pack rest
  _             => Nothing

toTurns : String -> List (Maybe Turn)
toTurns input = map toTurn $ lines input

getPassword1 : List (Maybe Turn) -> Maybe Nat
getPassword1 turns = go turns 50 0
  where
    go : List (Maybe Turn) -> Int -> Nat -> Maybe Nat
    go []                       _     val = Just val
    go (Nothing :: _)           _     _   = Nothing
    go (Just (L turn) :: turns) start val =
      let end    := start - turn
          newVal := if end `mod` 100 == 0 then val + 1 else val
       in go turns end newVal
    go (Just (R turn) :: turns) start val =
      let end    := start + turn
          newVal := if end `mod` 100 == 0 then val + 1 else val
       in go turns end newVal

export
solve2025D1P1 : String -> Maybe Nat
solve2025D1P1 = getPassword1 . toTurns

snapDown : Int -> Int
snapDown x = if x `mod` 100 == 0 then x else x - (x `mod` 100)

countPast100 : Int -> Int -> Int
countPast100 start end = ((snapDown end) - (snapDown start)) `div` 100

snapUp : Int -> Int
snapUp x = if x `mod` 100 == 0 then x else x + 100 - (x `mod` 100)

countDown100 : Int -> Int -> Int
countDown100 start end = ((snapUp start) - (snapUp end)) `div` 100

getPassword2 : List (Maybe Turn) -> Maybe Nat
getPassword2 turns = go turns 50 0
  where
    go : List (Maybe Turn) -> Int -> Nat -> Maybe Nat
    go []                       _     val = Just val
    go (Nothing :: _)           _     _   = Nothing
    go (Just (L turn) :: turns) start val =
      let end    := start - turn
          count  := countDown100 start end
          newVal := val + cast count
       in go turns end newVal
    go (Just (R turn) :: turns) start val =
      let end    := start + turn
          count  := countPast100 start end
          newVal := val + cast count
       in go turns end newVal

export
solve2025D1P2 : String -> Maybe Nat
solve2025D1P2 = getPassword2 . toTurns
