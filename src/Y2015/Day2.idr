module Y2015.Day2

import Data.String
import Parser

record Box where
  constructor MkBox
  l : Int
  w : Int
  h : Int

parseBox : Parser Box
parseBox = MkBox <$> (pInt) <*> (pChar 'x' *> pInt) <*> (pChar 'x' *> pInt)

findWrappingPaperArea : Box -> Int
findWrappingPaperArea (MkBox l w h) =
  let smallestFaceArea = min (l * w) (min (l * h) (w * h))
   in smallestFaceArea + 2 * l * w + 2 * w * h + 2 * l * h

export
solve2015D2P1 : String -> Either String Int
solve2015D2P1 input = do
  boxes <- sequence $ map (unifyParseOutput parseBox . unpack) $ lines $ trim input
  pure $ foldl (\prevSum, box => prevSum + findWrappingPaperArea box) 0 boxes

findRibbonLength : Box -> Int
findRibbonLength (MkBox l w h) =
  let smallestPerimeter = min (l + h) (min (l + w) (w + h))
   in smallestPerimeter * 2 + l * w * h

export
solve2015D2P2 : String -> Either String Int
solve2015D2P2 input = do
  boxes <- sequence $ map (unifyParseOutput parseBox . unpack) $ lines $ trim input
  pure $ foldl (\prevSum, box => prevSum + findRibbonLength box) 0 boxes
