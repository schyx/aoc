module Y2015.Day3

import Data.SortedSet
import Data.String
import Utils

data Direction = U | D | L | R

toDirection : Char -> Either String Direction
toDirection '^' = Right U
toDirection 'v' = Right D
toDirection '<' = Right L
toDirection '>' = Right R
toDirection c   = Left $ "Unsupported character: " ++ singleton c

applyMove : Direction -> (Int, Int) -> (Int, Int)
applyMove U (x, y) = (x, y + 1)
applyMove D (x, y) = (x, y - 1)
applyMove L (x, y) = (x - 1, y)
applyMove R (x, y) = (x + 1, y)

updateF1 : (SortedSet (Int, Int), (Int, Int)) -> Direction -> (SortedSet (Int, Int), (Int, Int))
updateF1 (seenPoints, point) dir =
  let newPoint = applyMove dir point
   in (insert newPoint seenPoints, newPoint)

export
solve2015D3P1 : String -> Either String Int
solve2015D3P1 input = do
  moves <- sequence $ map (toDirection) $ unpack $ trim $ input
  let output = Sets.size $ Builtin.fst $ foldl updateF1 (singleton (0, 0), (0, 0)) moves
  pure $ cast output

updateF2 : (SortedSet (Int, Int), (Int, Int), (Int, Int), Bool)
        -> Direction
        -> (SortedSet (Int, Int), (Int, Int), (Int, Int), Bool)
updateF2 (seenPoints, santaPoint, robotPoint, santaMove) dir =
  let newSantaPoint = applyMove dir santaPoint
      newRobotPoint = applyMove dir robotPoint
      newSeenPoints = insert (if santaMove then newSantaPoint else newRobotPoint) seenPoints
   in ( newSeenPoints
      , if santaMove then newSantaPoint else santaPoint
      , if santaMove then robotPoint else newRobotPoint
      , not santaMove
      )

export
solve2015D3P2 : String -> Either String Int
solve2015D3P2 input = do
  moves <- sequence $ map (toDirection) $ unpack $ trim $ input
  let output = Sets.size $ Builtin.fst $ foldl updateF2 (singleton (0, 0), (0, 0), (0, 0), False) moves
  pure $ cast output
