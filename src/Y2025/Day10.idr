module Y2025.Day10

import Data.Fin
import Data.SortedMap
import Data.String
import Data.Vect
import Parser
import Queue
import Utils

record Manual m where
  constructor MkManual
  target  : Vect m Bool
  flips   : SortedMap Int (List (Fin m))
  joltage : Vect m Int

Show (Manual m) where
  show manual =
    "(MkManual "
      ++ show manual.target
      ++ ", "
      ++ show manual.flips
      ++ ", "
      ++ show manual.joltage
      ++ ")"

data Wrapper : Type where
  MkWrapper : Manual m -> Wrapper

findSize : String -> Nat
findSize input = length $ filter (/= '[') $ fst $ span (/= ']') $ unpack input

parseLights : (m : Nat) -> Parser (Maybe (Vect m Bool))
parseLights m = toVect m . map (== '#') <$> (pChar '[' *> pSpan (/= ']') <* pChar ']')

parseFlip : (m : Nat) -> Parser (Maybe (List (Fin m)))
parseFlip m = sequence <$> (pChar '(' *> ((::) <$> pFin m <*> (many (pChar ',' *> pFin m))) <* pChar ')' <* pChar ' ')

parseFlips : (m : Nat) -> Parser (Maybe (SortedMap Int (List (Fin m))))
parseFlips m = (map toIndexedMap) . sequence <$> many (parseFlip m)

parseJoltages : (m : Nat) -> Parser (Maybe (Vect m Int))
parseJoltages m = toVect m <$> (pChar '{' *> ((::) <$> pInt <*> many (pChar ',' *> pInt)) <* pChar '}')

parseManual : (m : Nat) -> Parser (Maybe (Manual m))
parseManual m = do
  lights <- parseLights m
  const () <$> pChar ' '
  flips <- parseFlips m
  joltages <- parseJoltages m
  pure $ MkManual <$> lights <*> flips <*> joltages

unifyParseOutput : Maybe (Maybe (Manual m), List Char) -> Either String (Manual m)
unifyParseOutput Nothing              = Left "Unable to parse input"
unifyParseOutput (Just (_, chars@(_ :: _))) = Left $ "Leftover chars after parsing: " ++ pack chars
unifyParseOutput (Just (Nothing, [])) = Left "Parsing error!"
unifyParseOutput (Just (Just m, []))  = Right m

record FlipState m where
  constructor MkFlipState
  moves : List Int
  state : Vect m Bool

applyFlips : List (Fin m) -> Vect m Bool -> Vect m Bool
applyFlips flips state = foldl (\prevState, flip => updateAt flip not prevState) state flips

manualFlips : Manual m -> Int
manualFlips (MkManual target flips _) = dfs $ singleton ([], map (const False) target)
  where
    dfs : Queue (List Int, Vect m Bool) -> Int
    dfs states = case pop states of
      Nothing                                        => -11111111111
      Just ((prevMoves, currentState), poppedStates) =>
        let numFlips = the Int $ cast $ size flips
            prevMove = case head' prevMoves of
              Nothing  => the Int $ -1
              Just val => val
            possibleMoves =
              if prevMove + 1 == numFlips
                 then the (List Int) []
                 else [prevMove + 1 .. numFlips - 1]
            newStates = map (\move => let flips := case lookup move flips of 
                                                    Nothing => the (List (Fin _)) []
                                                    Just f  => f
                                       in (move :: prevMoves, applyFlips flips currentState)) possibleMoves
         in if any ((== target) . Builtin.snd) newStates
               then cast $ length prevMoves + 1
               else dfs $ foldr push poppedStates newStates

leastFlips : Wrapper -> Int
leastFlips (MkWrapper manual@(MkManual target flips _)) =
  if all (== False) target
    then 0
    else manualFlips manual

export
solve2025D10P1 : String -> Either String Int
solve2025D10P1 input = do
  let ls             = lines input
      sizeAndChars   = map (\s => (findSize s, unpack s)) ls
      eitherWrappers = map
        (\(size, chars) => MkWrapper <$> (unifyParseOutput $ runParser (parseManual size) chars))
        sizeAndChars
  wrappers <- sequence eitherWrappers
  pure $ foldl (\prev, wrapper => prev + leastFlips wrapper) 0 wrappers

applyMove : (List (Fin m)) -> Vect m Int -> Vect m Int
applyMove incs state = foldl (\prevState, inc => updateAt inc (+1) prevState) state incs

||| Subtrace left from right
subtractFrom : Vect m Int -> Vect m Int -> Vect m Int
subtractFrom l r = zipWith (-) r l

makeParityTable : List (List (Fin m)) -> Vect m Int -> SortedMap (Vect m Bool) (SortedMap (Vect m Int) Int)
makeParityTable presses target =
  go
    (singleton
      (map (const False) target)
      (singleton (map (const 0) target) 0))
    presses
  where
    go : SortedMap (Vect m Bool) (SortedMap (Vect m Int) Int)
      -> (List (List (Fin m)))
      -> SortedMap (Vect m Bool) (SortedMap (Vect m Int) Int)
    go table []              = table
    go table (move :: moves) =
      let asList        = kvList table
          appliedParity = map (\(k, v) => (applyFlips move k, v)) asList
          innerAsList   = map (\(k, v) => (k, kvList v)) appliedParity
          mapF : (Vect m Int, Int) -> (Vect m Int, Int)
          mapF (state, count) = (applyMove move state, count + 1)
          innerMapped   = map (\(k, v) => (k, map mapF v)) innerAsList
          innerMaps     = map (\(k, v) => (k, SortedMap.fromList v)) innerMapped
          moveApplied   = SortedMap.fromList innerMaps
       in go (mergeWith (\tOld, tNew => mergeWith min tOld tNew) table moveApplied) moves

manualPresses : Manual m -> Int
manualPresses (MkManual _ presses target) = Builtin.fst $ go target $ singleton (map (const 0) target) 0
  where
    parityTable = makeParityTable (values presses) target
    go : Vect m Int -> SortedMap (Vect m Int) Int -> (Int, SortedMap (Vect m Int) Int)
    go t table =
      let tParity      = map ((== 1) . (`mod` 2))  t
          moves        = case lookup tParity parityTable of
            Nothing => Prelude.Nil
            Just ms => kvList ms
          appliedMoves = map (\(k, v) => (subtractFrom k t, v)) moves
          pruned       = filter (\(state, _) => all (>= 0) state) appliedMoves
       in case lookup t table of
                 Just cached => (cached, table)
                 Nothing =>
                   let (value, unaddedTable) = foldl
                         (\(prevMin, prevTable), (state, stepsToState) =>
                           let (foundVal, newTable) = go (map (`div` 2) state) prevTable
                            in (min prevMin $ (2 * foundVal) + stepsToState, newTable)
                         )
                         (the Int 10000000, table)
                         pruned
                    in (value, insert t value unaddedTable)

leastPresses : Wrapper -> Int
leastPresses (MkWrapper manual@(MkManual _ _ target)) =
  if all (== 0) target
     then 0
     else manualPresses manual

||| Solution idea from https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/
export
solve2025D10P2 : String -> Either String Int
solve2025D10P2 input = do
  let ls             = lines input
      sizeAndChars   = map (\s => (findSize s, unpack s)) ls
      eitherWrappers = map
        (\(size, chars) => MkWrapper <$> (unifyParseOutput $ runParser (parseManual size) chars))
        sizeAndChars
  wrappers <- sequence eitherWrappers
  pure $ foldl (\prev, wrapper => prev + leastPresses wrapper) 0 wrappers
