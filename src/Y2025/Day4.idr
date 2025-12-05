module Y2025.Day4

import Data.SortedMap
import Data.SortedSet
import Data.String

%default total

record Coordinate where
  constructor MkCoordinate
  x : Int
  y : Int

Eq Coordinate where
  MkCoordinate x1 y1 == MkCoordinate x2 y2 = (x1, y1) == (x2, y2)

Ord Coordinate where
  compare (MkCoordinate x1 y1) (MkCoordinate x2 y2) = compare (x1, y1) (x2, y2)

Show Coordinate where
  show (MkCoordinate x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

insertCoord : Coordinate -> SortedMap Coordinate Int -> SortedMap Coordinate Int
insertCoord coord coords = insert coord (cast $ length coordsNeighbors) updatedNeighbors
  where
    coordsNeighbors =
      filter
        (not . null . lookup' coords)
        [
          MkCoordinate (coord.x - 1) (coord.y - 1),
          MkCoordinate (coord.x - 1) (coord.y),
          MkCoordinate (coord.x) (coord.y - 1),
          MkCoordinate (coord.x + 1) (coord.y - 1)
        ]
    updatedNeighbors = foldr (updateExisting (+1)) coords coordsNeighbors

coordsToNeighbors : List Char -> SortedMap Coordinate Int
coordsToNeighbors chars = go (MkCoordinate 1 1) chars empty
  where
    go : Coordinate -> List Char -> SortedMap Coordinate Int -> SortedMap Coordinate Int
    go _     []              coords = coords
    go coord ('\n' :: chars) coords = go (MkCoordinate 1 $ coord.y + 1) chars coords
    go coord ('@' :: chars)  coords = go (MkCoordinate (coord.x + 1) coord.y) chars $ insertCoord coord coords
    go coord (_ :: chars)    coords = go (MkCoordinate (coord.x + 1) coord.y) chars coords

export
solve2025D4P1 : String -> Int
solve2025D4P1 input = cast $ length $ filter (< 4) $ values $ coordsToNeighbors $ unpack input

step : SortedMap Coordinate Int -> SortedMap Coordinate Int
step input = foldr remove input toRemove
  where
    toRemove = map fst $ filter ((< 4) . snd) $ kvList input
    remove : Coordinate -> SortedMap Coordinate Int -> SortedMap Coordinate Int
    remove coord coords = decrement $ delete coord coords
      where
        decrement : SortedMap Coordinate Int -> SortedMap Coordinate Int
        decrement toDec = foldr
          (updateExisting (\v => v - 1))
          toDec
          [
            MkCoordinate (coord.x - 1) (coord.y - 1),
            MkCoordinate (coord.x - 1) (coord.y),
            MkCoordinate (coord.x) (coord.y - 1),
            MkCoordinate (coord.x + 1) (coord.y - 1),
            MkCoordinate (coord.x - 1) (coord.y + 1),
            MkCoordinate (coord.x + 1) (coord.y),
            MkCoordinate (coord.x) (coord.y + 1),
            MkCoordinate (coord.x + 1) (coord.y + 1)
          ]


export covering
solve2025D4P2 : String -> Int
solve2025D4P2 input = cast (length (kvList coords)) - cast (length (kvList $ repeatStep coords))
  where
    coords = coordsToNeighbors $ unpack input
    repeatStep : SortedMap Coordinate Int -> SortedMap Coordinate Int
    repeatStep x = let stepped := step x in if stepped == x then stepped else repeatStep stepped
