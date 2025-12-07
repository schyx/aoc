module Y2025.Day6

import Data.List1
import Data.String
import Data.Vect

data Operation = Plus | Times

record Problem (n : Nat) where
  constructor MkProblem
  numbers   : Vect n Int
  operation : Operation

solveProblem : Problem _ -> Int
solveProblem (MkProblem nums Plus)  = sum nums
solveProblem (MkProblem nums Times) = product nums

toProblems : Vect m String -> Vect n (Vect m String) -> Vect m (Problem n)
toProblems []           _      = []
toProblems (_ :: ops)   []     = MkProblem [] Plus :: toProblems ops []
toProblems ("+" :: ops) matrix = MkProblem (map (cast . head) matrix) Plus :: toProblems ops (map tail matrix)
toProblems ("*" :: ops) matrix = MkProblem (map (cast . head) matrix) Times :: toProblems ops (map tail matrix)
toProblems (_ :: ops)   matrix = MkProblem (map (cast . head) matrix) Plus :: toProblems ops (map tail matrix)

export
solve2025D6P1 : String -> Maybe Int
solve2025D6P1 input = foldr (\prob, prevSum => solveProblem prob + prevSum) 0 <$> (toProblems <$> operations <*> grid)
  where
    parsed = words <$> lines input
    vectLength =
      case head' parsed of
        Just v  => length v
        Nothing => 0
    operations = last' parsed >>= toVect vectLength
    grid = init' parsed >>= sequence . map (toVect vectLength) >>= toVect (pred $ length parsed)

record Problem2 where
  constructor MkProblem2
  numbers   : List Int
  operation : Operation

solveProblem2 : Problem2 -> Int
solveProblem2 (MkProblem2 nums Plus)  = sum nums
solveProblem2 (MkProblem2 nums Times) = product nums

toCephalopodProblems : {m: _} -> Vect n (Vect m Char) -> List Problem2
toCephalopodProblems vects = toList $ map toProblem2 rawProbs
  where
    rawProbs = split (== (replicate m ' ')) $ toList vects
    toProblem2 : List (Vect m Char) -> Problem2
    toProblem2 []           = MkProblem2 [] Plus
    toProblem2 (v1 :: rest) =
      let l   = toList v1
          op  = case last' l of
                  Just '+' => Plus
                  Just '*' => Times
                  _        => Plus
          v1' = case init' l of
                  Just v  => v
                  Nothing => ['0']
       in MkProblem2 (map (cast . trim . pack) (v1' :: (map toList rest))) op

export
solve2025D6P2 : String -> Maybe Int
solve2025D6P2 input = foldr (\prob, prevSum => solveProblem2 prob + prevSum) 0 <$> (toCephalopodProblems <$> vects)
  where
    ls         = lines input
    lineLength = case head' ls of
                   Just v  => length v
                   Nothing => 0
    height     = length ls
    vects      = transpose <$> (sequence (map (toVect lineLength . unpack) ls) >>= toVect height)
