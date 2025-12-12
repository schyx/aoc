module Y2025.Day11

import Data.SortedMap
import Data.SortedSet
import Data.String
import Parser
import Graph

parseParent : Parser String
parseParent = pack <$> pSpan (/= ':') <* pChar ':'

parseChildren : Parser (SortedSet String)
parseChildren = fromList <$> many (pChar ' ' *> pack <$> pSpan (/= ' '))

parseLine : Parser (String, SortedSet String)
parseLine = (,) <$> parseParent <*> parseChildren

unifyParseOutput : Maybe ((String, SortedSet String), List Char) -> Either String (String, SortedSet String)
unifyParseOutput Nothing                    = Left "Unable to parse input"
unifyParseOutput (Just (_, chars@(_ :: _))) = Left $ "Leftover chars after parsing: " ++ pack chars
unifyParseOutput (Just (val, []))           = Right val

export
solve2025D11P1 : String -> Either String Int
solve2025D11P1 input = do
  let ls = lines $ trim input
  tuples <- sequence $ map (unifyParseOutput . runParser parseLine . unpack) ls
  let graph = MkGraph $ foldl (\prevMap, (k, v) => insert k v prevMap) (singleton "out" SortedSet.empty) tuples
  reachableNodes <- reachableFrom "you" graph
  let reachableFromYou = filterGraph (`elem` reachableNodes) graph
  numPaths "you" "out" reachableFromYou

export
solve2025D11P2 : String -> Either String Int
solve2025D11P2 input = do
  let ls = lines $ trim input
  tuples <- sequence $ map (unifyParseOutput . runParser parseLine . unpack) ls
  let graph = MkGraph $ foldl (\prevMap, (k, v) => insert k v prevMap) (singleton "out" SortedSet.empty) tuples
  reachableNodes <- reachableFrom "svr" graph
  let reachableFromSvr = filterGraph (`elem` reachableNodes) graph
  forward <- (*) <$> ((*) <$> numPaths "svr" "fft" reachableFromSvr <*> numPaths "fft" "dac" reachableFromSvr) <*> numPaths "dac" "out" reachableFromSvr
  backward <- (*) <$> ((*) <$> numPaths "svr" "dac" reachableFromSvr <*> numPaths "dac" "fft" reachableFromSvr) <*> numPaths "fft" "out" reachableFromSvr
  pure $ forward + backward
  
