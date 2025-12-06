module Y2025.Day5

import Data.String
import Data.List1
import Debug.Trace

record Range where
  constructor MkRange
  low  : Int
  high : Int

makeRange : String -> Range
makeRange s = case split (== '-') s of
  (l ::: h :: []) => MkRange (cast l) (cast h)
  _               => MkRange 0 0

inAnyRange : List Range -> Int -> Bool
inAnyRange ranges id = any (\(MkRange l h) => (l <= id) && (id <= h)) ranges

export
solve2025D5P1 : String -> Int
solve2025D5P1 input =
  let (rangeStrings, ingredients) := span (/= "") $ lines $ trim input
      ranges                      := map makeRange rangeStrings
      ids                         := map (\v => the Int $ cast v) ingredients
   in cast $ length $ filter (inAnyRange ranges) ids

intersecting : Range -> Range -> Bool
intersecting r1 r2 = not $ (r1.high < r2.low) || (r2.high < r1.low)

combine : Range -> Range -> Range
combine r1 r2 = MkRange (min r1.low r2.low) (max r1.high r2.high)

-- After adding, all ranges should be nonintersecting
addRange : Range -> List Range -> List Range
addRange range ranges = go range ranges []
  where
    go : Range -> List Range -> List Range -> List Range
    go toAdd []         buildup = toAdd :: buildup
    go toAdd (r1 :: rs) buildup =
      if intersecting r1 toAdd
      then go (combine r1 toAdd) (buildup ++ rs) []
      else go toAdd rs (r1 :: buildup)

export
solve2025D5P2 : String -> Int
solve2025D5P2 input =
  let rangeStrings := fst $ span (/= "") $ lines $ input
      finalRanges  := foldr (\rs, ranges => addRange (makeRange rs) ranges) [] rangeStrings
   in foldr (\r, buildup => r.high - r.low + 1 + buildup) 0 finalRanges
