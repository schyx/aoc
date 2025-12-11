module Y2025.Day10

import Debug.Trace
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

applyButtonPress : List (Fin m) -> Vect m Int -> Vect m Int
applyButtonPress presses state = foldl (\prevState, press => updateAt press (+1) prevState) state presses

manualPresses : Manual m -> Int
manualPresses (MkManual _ presses target) = traceVal $ dp (singleton (map (const 0) target) 0) (singleton (map (const 0) target))
  where
    difference : Vect m Int -> Vect m Int
    difference = zipWith (-) target
    dp : SortedMap (Vect m Int) Int -> Queue (Vect m Int) -> Int
    dp table queue = case pop queue of
      Nothing                   => -11111111111
      Just (state, poppedQueue) =>
        let allPresses = values presses
            numMoves   = case lookup state table of
              Nothing => cast $ -11111111111
              Just n  => n
            allNewStates = map (\ps => applyButtonPress ps state) allPresses
            unseenNewStates = filter (\newState => case lookup newState table of
                                        Just _ => False
                                        _      => True
                                ) allNewStates
            nonLargerNewStates = filter (\newState => all id $ zipWith (<=) newState target) unseenNewStates
            newTable = foldl
              (\table', newState => insert newState (numMoves + 1) table')
              table
              nonLargerNewStates
            newQueue = foldr push poppedQueue nonLargerNewStates
            complementSteps = foldl
              (\prevMaybe, newState => case prevMaybe of
                Just v  => Just v
                Nothing => lookup (difference newState) table
              ) Nothing nonLargerNewStates
         in case complementSteps of
                 Nothing => dp newTable newQueue
                 Just v  => v + numMoves + 1

leastPresses : Wrapper -> Int
leastPresses (MkWrapper manual@(MkManual _ _ target)) =
  if all (== 0) target
     then 0
     else manualPresses manual

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
