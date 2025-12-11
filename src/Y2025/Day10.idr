module Y2025.Day10

import Data.SortedMap
import Data.String
import Parser
import Queue
import Utils

record Manual where
  constructor MkManual
  target  : SortedMap Int Bool
  flips   : SortedMap Int (List Int)
  joltage : SortedMap Int Int

Show Manual where
  show manual =
    "(MkManual "
      ++ show manual.target
      ++ ", "
      ++ show manual.flips
      ++ ", "
      ++ show manual.joltage
      ++ ")"

parseTargets : Parser (SortedMap Int Bool)
parseTargets =
  Builtin.fst
    . foldl
        (\(prevMap, idx), char => (insert idx (char == '#') prevMap, idx + 1))
        (empty, 0)
        <$> (pChar '[' *> pSpan (/= ']') <* pChar ']')

parseFlip : Parser (List Int)
parseFlip = pChar '(' *> ((::) <$> (pInt) <*> (many (pChar ',' *> pInt))) <* pChar ')' <* pChar ' '

parseFlips : Parser (SortedMap Int (List Int))
parseFlips = toIndexedMap <$> many parseFlip

parseJoltages : Parser (SortedMap Int Int)
parseJoltages = toIndexedMap <$> (pChar '{' *> ((::) <$> pInt <*> many (pChar ',' *> pInt)) <* pChar '}')

parseManual : Parser Manual
parseManual = MkManual <$> (parseTargets <* pChar ' ') <*> parseFlips <*> parseJoltages

unifyParseOutput : Maybe (Manual, List Char) -> Either String Manual
unifyParseOutput Nothing              = Left "Unable to parse input"
unifyParseOutput (Just (_, (_ :: _))) = Left "Leftover chars after parsing"
unifyParseOutput (Just (manual, []))  = Right manual

record FlipState where
  constructor MkFlipState
  moves : List Int
  state : SortedMap Int Bool

Show FlipState where
  show (MkFlipState moves state) = "(MkFlipState " ++ show moves ++ " " ++ show state ++ ")"

applyFlips : List Int -> SortedMap Int Bool -> SortedMap Int Bool
applyFlips flips state = foldl (\oldState, flip => updateExisting not flip oldState) state flips

leastFlips : Manual -> Int
leastFlips (MkManual target flips _) =
  if all (== False) $ values target
    then 0
    else go $ singleton (MkFlipState [] $ map (const False) target)
  where
    numFlips = the Int $ cast $ size flips
    go : Queue FlipState -> Int
    go states = case pop states of
      Nothing                                                 => -111111111111111
      Just ((MkFlipState prevMoves lightState), poppedStates) =>
        let prevMove = case head' prevMoves of
              Nothing  => -1
              Just val => val
            possibleMoves =
              if prevMove + 1 == numFlips
                then the (List Int) []
                else [prevMove + 1 .. numFlips - 1]
            newStates = map
              (\move => let flips := case lookup move flips of
                                       Nothing => the (List Int) []
                                       Just f  => f
                         in MkFlipState (move :: prevMoves) (applyFlips flips lightState)
              )
              possibleMoves
         in if any ((== target) . state) newStates
              then cast $ length prevMoves + 1
              else go $ foldr push poppedStates newStates

export
solve2025D10P1 : String -> Either String Int
solve2025D10P1 input = do
  manuals <- sequence $ map (unifyParseOutput . runParser parseManual . unpack) $ lines input
  pure $ foldl (\prev, manual => prev + leastFlips manual) 0 manuals

applyButtonPress : List Int -> SortedMap Int Int -> SortedMap Int Int
applyButtonPress flips state = foldl (\oldState, flip => updateExisting (+1) flip oldState) state flips

record JoltageState where
  constructor MkJoltageState
  lastMove     : Int
  moves        : Int
  joltageState : SortedMap Int Int

Show JoltageState where
  show (MkJoltageState lastMove moves state) = "(MkJoltageState " ++ show lastMove ++ " " ++ show moves ++ " " ++ show state ++ ")"

lessThanTarget : SortedMap Int Int -> SortedMap Int Int -> Bool
lessThanTarget target source =
  all (\idx => lookup idx source <= lookup idx target) $ keys target

leastPresses : Manual -> Int
leastPresses (MkManual _ flips targetJoltages) =
  if all (== 0) $ values targetJoltages
    then 0
    else go $ singleton $ MkJoltageState 0 0 $ map (const 0) targetJoltages
  where
    numFlips = the Int $ cast $ size flips
    go : Queue JoltageState -> Int
    go states = case pop states of
      Nothing                                                    => -111111111111111
      Just ((MkJoltageState lastMove numMoves lightState), poppedStates) =>
        let possibleMoves = [lastMove .. numFlips - 1]
            newStates = filter (\js => lessThanTarget targetJoltages $ joltageState js) $ map
              (\move => let flips := case lookup move flips of
                                       Nothing => the (List Int) []
                                       Just f  => f
                         in MkJoltageState move (numMoves + 1) (applyButtonPress flips lightState)
              )
              possibleMoves
         in if any ((== targetJoltages) . joltageState) newStates
              then cast $ numMoves + 1
              else go $ foldr push poppedStates newStates

export
solve2025D10P2 : String -> Either String Int
solve2025D10P2 input = do
  manuals <- sequence $ map (unifyParseOutput . runParser parseManual . unpack) $ lines input
  pure $ foldl (\prev, manual => prev + leastPresses manual) 0 manuals
