module Y2025.Day12

import Data.String
import Data.Vect
import Parser
import Utils

record Shape where
  constructor MkShape
  size  : Int
  cells : Vect 3 (Vect 3 Bool)

createShape : List String -> Shape
createShape ls =
  let shape   = map unpack $ take 3 $ drop 1 ls
      count   = foldl (\prevSum, chars => prevSum + cast (length (filter (== '#') chars))) 0 shape
      toVect3 : List Char -> Vect 3 Bool
      toVect3 chars = case toVect 3 chars of
                           Just cs => map (== '#') cs
                           _       => replicate 3 False
      rows    = case shape of
                   [r1, r2, r3] => the (Vect 3 (Vect 3 Bool)) [toVect3 r1, toVect3 r2, toVect3 r3]
                   _            => Vect.replicate 3 $ Vect.replicate 3 False
   in MkShape count rows

createShapes : (n : Nat) -> List String -> Vect n Shape
createShapes Z     _  = Nil
createShapes (S n) ls = createShape (take 5 ls) :: createShapes n (drop 5 ls)

record Problem where
  constructor MkProblem
  width   : Int
  height  : Int
  objects : Vect 6 Int

parseCounts : Parser (Vect 6 Int)
parseCounts = do
  ints <- many (pChar ' ' *> pInt)
  case toVect 6 ints of
       Just v => pure v
       _      => pure $ Vect.replicate 6 0

createProblem : Parser Problem
createProblem = MkProblem <$> (pInt <* pChar 'x') <*> (pInt <* pChar ':') <*> parseCounts

unifyOutput : Maybe (Problem, List Char) -> Either String Problem
unifyOutput Nothing                    = Left $ "parse error"
unifyOutput (Just (_, chars@(_ :: _))) = Left ("leftover chars: " ++ pack chars)
unifyOutput (Just (p, _))              = Right p

createProblems : List String -> Either String (List Problem)
createProblems ls = sequence $ map (unifyOutput . runParser createProblem . unpack) ls

solveProblem : Vect 6 Shape -> Problem -> Bool
solveProblem shapes (MkProblem w h os) =
  if      dotProd (map size shapes) os >= w * h then False
  else if sum os <= (w `div` 3 * h `div` 3)     then True
  else                                               ?hole

export
solve2025D12P1 : String -> Either String Int
solve2025D12P1 input = do
  let ls = lines $ trim input
      shapes = createShapes 6 $ take 30 ls
  problems <- createProblems $ drop 30 ls
  pure $ cast $ length $ filter (solveProblem shapes) problems

export
solve2025D12P2 : String -> String
solve2025D12P2 = const "Yay!"
