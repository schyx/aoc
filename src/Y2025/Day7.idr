module Y2025.Day7

import Utils

import Data.SortedMap
import Data.SortedSet
import Data.String
import Data.Vect

tachyonSplits : List (List Char) -> Int
tachyonSplits ls = snd $ foldl step (empty, 0) ls
  where
    go : (SortedSet Int, Int) -> List Char -> Int -> (SortedSet Int, Int)
    go prev                 []             _   = prev
    go (beamSet, numSplits) ('S' :: chars) idx = go (insert idx beamSet, numSplits) chars (idx + 1)
    go (beamSet, numSplits) ('^' :: chars) idx = go (
      if contains idx beamSet
        then (insert (idx + 1) $ insert (idx - 1) $ delete idx beamSet, numSplits + 1)
        else (beamSet, numSplits)
      ) chars (idx + 1)
    go prev                 (_ :: chars)   idx = go prev chars (idx + 1)
    step : (SortedSet Int, Int) -> List Char -> (SortedSet Int, Int)
    step prev chars = go prev chars 0

export
solve2025D7P1 : String -> Int
solve2025D7P1 input = tachyonSplits $ map unpack $ words input

tachyonPaths : List (List Char) -> Int
tachyonPaths ls = foldr (+) 0 $ foldl step empty ls
  where
    updateHelper : Int -> Maybe Int -> Maybe Int
    updateHelper seed Nothing    = Just seed
    updateHelper seed (Just val) = Just $ seed + val
    go : SortedMap Int Int -> List Char -> Int -> SortedMap Int Int
    go prev []             _   = prev
    go prev ('S' :: chars) idx = go (insert idx 1 prev) chars (idx + 1)
    go prev ('^' :: chars) idx = go (
      case lookup idx prev of
           Just count => update (updateHelper count) (idx - 1)
                           $ update (updateHelper count) (idx + 1)
                           $ delete idx prev
           Nothing    => prev
      ) chars (idx + 1)
    go prev (_ :: chars)   idx = go prev chars (idx + 1)
    step : SortedMap Int Int -> List Char -> SortedMap Int Int
    step prev chars = go prev chars 0

export
solve2025D7P2 : String -> Int
solve2025D7P2 input = tachyonPaths $ map unpack $ words input
