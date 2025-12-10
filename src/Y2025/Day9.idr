module Y2025.Day9

import Data.Either
import Data.List1
import Data.SortedMap
import Data.SortedSet
import Data.String

record Coordinate where
  constructor MkCoordinate
  x : Int
  y : Int

parseCoordinate : String -> Either String Coordinate
parseCoordinate l =
  case split (== ',') l of
    (xVal ::: yVal :: []) => Right $ MkCoordinate (cast xVal) (cast yVal)
    _                     => Left $ "Cannot parse line: " ++ l

rectArea : Coordinate -> Coordinate -> Int
rectArea (MkCoordinate x1 y1) (MkCoordinate x2 y2) =
  (1 + abs (x1 - x2)) * (1 + abs (y1 - y2))

toDistanceHeap : List Coordinate -> SortedMap Int (Coordinate, Coordinate)
toDistanceHeap coords = go coords empty
  where
    go : List Coordinate -> SortedMap Int (Coordinate, Coordinate) -> SortedMap Int (Coordinate, Coordinate)
    go []        buildup = buildup
    go (c :: cs) buildup = go cs $ foldl (\prev, c' => insert (rectArea c c') (c, c') prev) buildup cs

export
solve2025D9P1 : String -> Either String Int
solve2025D9P1 input = do
  coords <- sequence $ map parseCoordinate $ lines $ trim input
  let heap = toDistanceHeap coords
  largest <- maybeToEither "Empty distance heap" $ rightMost heap
  pure $ fst largest

export
solve2025D9P2 : String -> Either String Int
solve2025D9P2 input = do
  coords <- sequence $ map parseCoordinate $ lines $ trim input
  ?d2
