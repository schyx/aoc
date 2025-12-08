module Y2025.Day8

import Data.Either
import Data.List1
import Data.SortedMap
import Data.String

record Coordinate where
  constructor MkCoordinate
  x : Int
  y : Int
  z : Int

Eq Coordinate where
  MkCoordinate x1 y1 z1 == MkCoordinate x2 y2 z2 = (x1, y1, z1) == (x2, y2, z2)

Ord Coordinate where
  compare (MkCoordinate x1 y1 z1) (MkCoordinate x2 y2 z2) = compare (x1, y1, z1) (x2, y2, z2)

Show Coordinate where
  show (MkCoordinate x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

record DSValue where
  constructor MkDSValue
  parent : Maybe Coordinate
  size   : Int

Show DSValue where
  showPrec p dsv = showCon p "MkDSValue" $ show dsv.parent ++ ", " ++ show dsv.size

startDisjointSet : List Coordinate -> SortedMap Coordinate DSValue
startDisjointSet = foldl (\ds, coord => insert coord (MkDSValue Nothing 1) ds) empty

dsLookup : Coordinate -> SortedMap Coordinate DSValue -> Either String DSValue
dsLookup coord ds = maybeToEither ("Cannot find " ++ show coord ++ " in " ++ show ds) $ lookup coord ds

getRepresentative : Coordinate -> SortedMap Coordinate DSValue -> Either String (Coordinate, SortedMap Coordinate DSValue)
getRepresentative coord ds = do
  dsv <- dsLookup coord ds
  case dsv.parent of
    Nothing => Right (coord, ds)
    Just p  => do
      (rep, modifiedDs) <- getRepresentative p ds
      let outputDs = insert coord (MkDSValue (Just rep) dsv.size) modifiedDs
      pure (rep, outputDs)

mergeSets : Coordinate -> Coordinate -> SortedMap Coordinate DSValue -> Either String (SortedMap Coordinate DSValue)
mergeSets s1 s2 ds = do
  (r1, ds1) <- getRepresentative s1 ds
  (r2, ds2) <- getRepresentative s2 ds1
  if r1 == r2 then pure ds2 else do
    rep1 <- dsLookup r1 ds2
    rep2 <- dsLookup r2 ds2
    if size rep1 > size rep2
       then pure $ insert r1 (MkDSValue rep1.parent $ rep1.size + rep2.size) $ insert r2 (MkDSValue (Just r1) $ rep2.size) ds2
       else pure $ insert r2 (MkDSValue rep2.parent $ rep1.size + rep2.size) $ insert r1 (MkDSValue (Just r2) $ rep1.size) ds2

distance : Coordinate -> Coordinate -> Int
distance (MkCoordinate x1 y1 z1) (MkCoordinate x2 y2 z2) =
  ((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)) + ((z2 - z1) * (z2 - z1))

parseCoordinate : String -> Either String Coordinate
parseCoordinate line =
  case split (== ',') line of
    (xVal ::: yVal :: zVal :: []) => Right $ MkCoordinate (cast xVal) (cast yVal) (cast zVal)
    _                             => Left $ "Unable to parse " ++ line ++ " into coordinate"

toDistanceHeap : List Coordinate -> SortedMap Int (Coordinate, Coordinate)
toDistanceHeap coords = go coords empty
  where
    go : List Coordinate -> SortedMap Int (Coordinate, Coordinate) -> SortedMap Int (Coordinate, Coordinate)
    go []        buildup = buildup
    go (c :: cs) buildup = go cs $ foldl (\prev, c' => insert (distance c c') (c, c') prev) buildup cs

linkCircuits : Int -> List Coordinate -> Either String (SortedMap Coordinate DSValue)
linkCircuits count coords = snd <$> finalDS
  where
    distances = toDistanceHeap coords
    initialDS = startDisjointSet coords
    step : (SortedMap Int (Coordinate, Coordinate), SortedMap Coordinate DSValue)
        -> Either String Integer
        -> Either String (SortedMap Int (Coordinate, Coordinate), SortedMap Coordinate DSValue)
    step (dists, ds) _ = do
      ((_, (start, end)), newDists) <- maybeToEither ("Can't pop from an empty map") (pop dists)
      newDS <- mergeSets start end ds
      pure (newDists, newDS)
    finalDS = foldlM step (distances, initialDS) $ map Right [1 .. cast count]

multThreeLargest : SortedMap Coordinate DSValue -> Either String Int
multThreeLargest ds = Right $ product $ map size $ take 3 $ sortBy (compare `on` ((1-) . size)) $ values ds

export
solve2025D8P1 : String -> Either String Int
solve2025D8P1 input =
  (sequence $ map parseCoordinate $ lines $ trim input)
    >>= linkCircuits 1000
    >>= multThreeLargest

mergeSets2 : Coordinate -> Coordinate -> Int -> SortedMap Coordinate DSValue -> Either String (SortedMap Coordinate DSValue, Bool)
mergeSets2 s1 s2 i ds = do
  (r1, ds1) <- getRepresentative s1 ds
  (r2, ds2) <- getRepresentative s2 ds1
  if r1 == r2 then pure (ds2, False) else do
    rep1 <- dsLookup r1 ds2
    rep2 <- dsLookup r2 ds2
    let (smaller, smallerRep, larger, largerRep) =
          if size rep1 > size rep2
             then (r2, rep2, r1, rep1)
             else (r1, rep1, r2, rep2)
        afterSmallerChange = insert smaller (MkDSValue (Just larger) smallerRep.size) ds2
        afterLargerChange  = insert larger (MkDSValue Nothing $ smallerRep.size + largerRep.size) afterSmallerChange
        done = size rep1 + size rep2 == i
    pure (afterLargerChange, done)

repeatLink : List Coordinate -> Either String Int
repeatLink coords = go distances initialDS
  where
    distances = toDistanceHeap coords
    initialDS = startDisjointSet coords
    numCoords = the Int $ cast $ length coords
    go : SortedMap Int (Coordinate, Coordinate) -> SortedMap Coordinate DSValue -> Either String Int
    go dists ds = do
      ((_, (start, end)), newDists) <- maybeToEither ("Can't pop from an empty map") (pop dists)
      (newDS, done) <- mergeSets2 start end numCoords ds
      if done
         then pure $ start.x * end.x
         else go newDists newDS

export
solve2025D8P2 : String -> Either String Int
solve2025D8P2 input = (sequence $ map parseCoordinate $ lines $ trim input) >>= repeatLink
