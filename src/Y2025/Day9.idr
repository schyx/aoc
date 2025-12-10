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

Show Coordinate where
  show (MkCoordinate x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

Eq Coordinate where
  MkCoordinate x1 y1 == MkCoordinate x2 y2 = (x1, y1) == (x2, y2)

Ord Coordinate where
  compare (MkCoordinate x1 y1) (MkCoordinate x2 y2) = compare (x1, y1) (x2, y2)

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
    go (c :: cs) buildup = go cs $ foldl (\prev, c' => insert (negate $ rectArea c c') (c, c') prev) buildup cs

export
solve2025D9P1 : String -> Either String Int
solve2025D9P1 input = do
  coords <- sequence $ map parseCoordinate $ lines $ trim input
  let heap = toDistanceHeap coords
  largest <- maybeToEither "Empty distance heap" $ leftMost heap
  pure $ negate $ fst largest

data Line : Type where
  Vertical   : Int -> (Int, Int) -> Line
  Horizontal : (Int, Int) -> Int -> Line

Eq Line where
  Vertical x1 (ly1, hy1)   == Vertical x2 (ly2, hy2)   = (x1, ly1, hy1) == (x2, ly2, hy2)
  Horizontal (lx1, hx1) y1 == Horizontal (lx2, hx2) y2 = (lx1, hx1, y1) == (lx2, hx2, y2)
  _                        == _                        = False

Ord Line where
  compare (Vertical _ _)      (Horizontal _ _)    = LT
  compare (Horizontal _ _)    (Vertical _ _)      = GT
  compare (Vertical x1 ys1)   (Vertical x2 ys2)   = compare (x1, ys1) (x2, ys2)
  compare (Horizontal xs1 y1) (Horizontal xs2 y2) = compare (y1, xs1) (y2, xs2)

Show Line where
  show (Vertical x (ly, hy)) = "(Vertical " ++ show x ++ " (" ++ show ly ++ ", " ++ show hy ++ ")"
  show (Horizontal (lx, hx) y) = "(Horizontal (" ++ show lx ++ ", " ++ show hx ++ ") " ++ show y ++ ")"

makeLine : Coordinate -> Coordinate -> Either String Line
makeLine c1@(MkCoordinate x1 y1) c2@(MkCoordinate x2 y2) =
  if      x1 == x2 then Right $ Vertical x1 (min y1 y2, max y1 y2)
  else if y1 == y2 then Right $ Horizontal (min x1 x2, max x1 x2) y1
  else                  Left $ "Coordinates " ++ show c1 ++ " and " ++ show c2 ++ " are not in the same row or column."

makeLines : List Coordinate -> Either String (SortedMap Line ())
makeLines coords = do
  lastLine <- makeLine <$> (maybeToEither "empty list" $ head' coords) <*> (maybeToEither "empty list" $ last' coords)
  lastLineSet <- singleton <$> lastLine <*> pure ()
  go coords lastLineSet
  where
    go : List Coordinate -> SortedMap Line () -> Either String (SortedMap Line ())
    go (c1 :: c2 :: cs) buildup = do
      line <- makeLine c1 c2
      go (c2 :: cs) $ insert line () buildup
    go _ buildup = pure buildup

getBoundingBox : List Coordinate -> (Int, Int)
getBoundingBox coords = go (0, 0) coords
  where
    go : (Int, Int) -> List Coordinate -> (Int, Int)
    go bounds               []        = bounds
    go (biggestX, biggestY) (c :: cs) = go (max biggestX (c.x + 1), max biggestY (c.y + 1)) cs

isInBoundingLines : Coordinate -> SortedMap Line () -> Bool
isInBoundingLines (MkCoordinate cx cy) boundingLines =
  let verticalLine         = Vertical cx (cy, cy)
      horizontalLine       = Horizontal (cx, cx) cy
      intersectVertical : (Line, ()) -> Bool
      intersectVertical (Vertical xVal (lowY, highY), ()) = xVal == cx && lowY <= cy && cy <= highY
      intersectVertical (Horizontal _ _, ())              = False
      verticalIntersection =
        case lookupBetween verticalLine boundingLines of
            (Nothing, Nothing)        => False
            (Nothing, Just higher)    => intersectVertical higher
            (Just lower, Nothing)     => intersectVertical lower
            (Just lower, Just higher) => intersectVertical lower || intersectVertical higher
      intersectHorizontal : (Line, ()) -> Bool
      intersectHorizontal (Vertical _ _, ())                  = False
      intersectHorizontal (Horizontal (lowX, highX) yVal, ()) = yVal == cy && lowX <= cx && cx <= highX
      horizontalIntersection =
        case lookupBetween horizontalLine boundingLines of
            (Nothing, Nothing)        => False
            (Nothing, Just higher)    => intersectHorizontal higher
            (Just lower, Nothing)     => intersectHorizontal lower
            (Just lower, Just higher) => intersectHorizontal lower || intersectHorizontal higher
   in verticalIntersection || horizontalIntersection

||| By dfs starting at (0, 0)
getOuterPoints : (Int, Int) -> SortedMap Line () -> SortedSet Coordinate
getOuterPoints (maxX, maxY) boundingLines = dfs [MkCoordinate 0 0] $ singleton (MkCoordinate 0 0)
  where
    inBoundsX xVal = xVal >= 0 && xVal <= maxX
    inBoundsY yVal = yVal >= 0 && yVal <= maxY
    findNewNeighbors : Coordinate -> SortedSet Coordinate -> (List Coordinate, SortedSet Coordinate)
    findNewNeighbors (MkCoordinate cx cy) alreadyExisting =
      let possibleNeighbors := [
            MkCoordinate (cx + 1) (cy),
            MkCoordinate (cx - 1) (cy),
            MkCoordinate (cx) (cy + 1),
            MkCoordinate (cx) (cy - 1)
          ]
          filterF : Coordinate -> Bool
          filterF coord@(MkCoordinate fx fy) = all id
            [ inBoundsX fx
            , inBoundsY fy
            , not $ contains coord alreadyExisting
            , not $ isInBoundingLines coord boundingLines
            ]
          neighbors = filter filterF possibleNeighbors
       in (neighbors, union alreadyExisting $ fromList neighbors)
    dfs : List Coordinate -> SortedSet Coordinate -> SortedSet Coordinate
    dfs [] buildup        = buildup
    dfs (c :: cs) buildup =
      let (neighbors, newBuildup) := findNewNeighbors c buildup
       in dfs (neighbors ++ cs) newBuildup

largestContained : SortedSet Coordinate -> SortedMap Int (Coordinate, Coordinate) -> SortedMap Int Int -> SortedMap Int Int -> Either String Int
largestContained outerPoints distanceHeap indexedXs indexedYs = do
  result <- rect
  case result of
    Right num    => pure num
    Left newHeap => largestContained outerPoints newHeap indexedXs indexedYs
  where
    allInside : (Coordinate, Coordinate) -> Bool
    allInside (c1, c2) =
      all (not . contains' outerPoints) topSide
        && all (not . contains' outerPoints) botSide
        && all (not . contains' outerPoints) leftSide
        && all (not . contains' outerPoints) rightSide
      where
        newC1     = case (lookup c1.x indexedXs, lookup c1.y indexedYs) of
                         (Just ncx, Just ncy) => MkCoordinate ncx ncy
                         _                    => MkCoordinate 0 0
        newC2     = case (lookup c2.x indexedXs, lookup c2.y indexedYs) of
                         (Just ncx, Just ncy) => MkCoordinate ncx ncy
                         _                    => MkCoordinate 0 0
        topSide   = [MkCoordinate newC1.x yVal | yVal <- [newC1.y .. newC2.y]]
        botSide   = [MkCoordinate newC2.x yVal | yVal <- [newC1.y .. newC2.y]]
        leftSide  = [MkCoordinate xVal newC1.y | xVal <- [newC1.x .. newC2.x]]
        rightSide = [MkCoordinate xVal newC2.y | xVal <- [newC1.x .. newC2.x]]
    rect = case pop distanceHeap of
      Nothing                           => Left "Empty map"
      Just ((negArea, coords), newHeap) => Right $
        if allInside coords then the (Either (SortedMap Int (Coordinate, Coordinate)) Int) $ Right $ negate negArea
        else                     Left newHeap

condenseCoords : List Coordinate -> Either String (List Coordinate, SortedMap Int Int, SortedMap Int Int)
condenseCoords inputCoords =
  let (xs, ys) := foldl
          (\(xi, yi), (MkCoordinate xc yc) => (insert xc () xi, insert yc () yi))
          (singleton 0 (), singleton 0 ())
          inputCoords
      (orderedXs, orderedYs) = (kvList xs, kvList ys)
      (indexedXs, indexedYs) = (toIndex orderedXs, toIndex orderedYs)
   in (, indexedXs, indexedYs) <$> sequence
        (map
          (\(MkCoordinate mx my) => do
            newX <- maybeToEither "Cannot find x value" $ lookup mx indexedXs
            newY <- maybeToEither "Cannot find y value" $ lookup my indexedYs
            pure $ MkCoordinate newX newY
          )
          inputCoords
        )
  where
    toIndex : List (Int, ()) -> SortedMap Int Int
    toIndex ls = Builtin.fst $
      foldl (\(buildup, idx), (coordVal, ()) => (insert coordVal idx buildup, idx + 1)) (empty, 0) ls

||| Works by condensing the coordinate map to each coordinate's relative index
||| Then check for perimeter red-green behavior against the condensed map
export
solve2025D9P2 : String -> Either String Int
solve2025D9P2 input = do
  coords <- sequence $ map parseCoordinate $ lines $ trim input
  (condensedCoords, indexedXs, indexedYs) <- condenseCoords coords
  boundingLines <- makeLines condensedCoords
  let (largestX, largestY) := getBoundingBox condensedCoords
      outerPoints := getOuterPoints (largestX, largestY) boundingLines
      heap = toDistanceHeap coords
  largestContained outerPoints heap indexedXs indexedYs
