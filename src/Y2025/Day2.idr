module Y2025.Day2

import Data.List1
import Data.SortedSet
import Data.String

%default total

record Range where
  constructor MkRange
  start : Int
  end   : Int

Show Range where
  show (MkRange s e) = "MkRange " ++ (show s) ++ " " ++ (show e)

toRange : String -> Range
toRange input =
  let (l, r') := span isDigit input
      (_, r)  := span (== '-') r'
   in MkRange (cast l) (cast r)

numLength : Int -> Int
numLength x = cast $ length $ the String (cast x)

repeat9 : Int -> Int
repeat9 digits = cast $ replicate (cast digits) '9'

repeat10 : Int -> Int
repeat10 digits = 1 + repeat9 (digits - 1)

numsBetween : Int -> Int -> List Int
numsBetween l h =
  if h - l < 2
     then []
     else [l + 1 .. h - 1]

-- s and e have the same number of digits
sameLengthCopies1 : Int -> Int -> Int
sameLengthCopies1 s e =
  let numDigits   := numLength s
      divisorPart := "1" ++ (replicate (cast $ (numDigits `div` 2 - 1)) '0') ++ "1"
      divisor     := 2 + (repeat9 $ numDigits `div` 2)
      sMul        := if s `mod` divisor == 0 then s `div` divisor else (s `div` divisor) + 1
      eMul        := e `div` divisor
      output      := divisor * ((eMul - sMul + 1) * (eMul + sMul)) `div` 2
   in if numDigits `mod` 2 == 1 then 0 else output

sumCopies1 : Range -> Int
sumCopies1 (MkRange s e) =
  let sLen   := numLength s
      eLen   := numLength e
      sUpper := repeat9 sLen
      eLower := repeat10 eLen
   in if sLen == eLen then sameLengthCopies1 s e
      else sameLengthCopies1 s sUpper
            + sameLengthCopies1 eLower e
            + if sLen + 1 <= eLen
                 then foldr
                        (\digits, prev => prev + sameLengthCopies1 (repeat10 digits) (repeat9 digits))
                        0
                        $ numsBetween sLen eLen
                 else 0

export
solve2025D2P1 : String -> Int
solve2025D2P1 input = foldr (\s, i => (i+) $ sumCopies1 $ toRange s) 0 $ split (== ',') $ trim input

dividerFromLength : (segmentSize : Int) -> (digits : Int) -> Int
dividerFromLength segmentSize digits = 
  let repeats := the Nat $ cast $ (digits `div` segmentSize) - 1
      zeros   := replicate (the Nat $ cast $ segmentSize - 1) '0'
      source  := zeros ++ "1"
   in the Int $ cast $ "1" ++ (foldl (++) "" $ replicate repeats source)

go : Int -> Int -> Int -> SortedSet Int
go s e divider =
  let sMul := if s `mod` divider == 0 then s `div` divider else s `div` divider + 1
      eMul := e `div` divider
   in fromList $ map (* divider) $ numsBetween (sMul - 1) (eMul + 1)

sameLengthCopies2 : Int -> Int -> Int
sameLengthCopies2 s e = if numDigits == 1 then 0 else sum $ foldr (\div, prev => union prev $ go s e div) empty dividers
  where
    numDigits      = numLength s
    dividerLengths = filter ((== 0) . (mod numDigits)) [1 .. numDigits - 1]
    dividers       = map (\x => dividerFromLength x numDigits) dividerLengths

sumCopies2 : Range -> Int
sumCopies2 (MkRange s e) =
  let sLen   := numLength s
      eLen   := numLength e
      sUpper := repeat9 sLen
      eLower := repeat10 eLen
   in if sLen == eLen then sameLengthCopies2 s e
      else sameLengthCopies2 s sUpper
            + sameLengthCopies2 eLower e
            + if sLen + 1 <= eLen
                 then foldr
                        (\digits, prev => prev + sameLengthCopies2 (repeat10 digits) (repeat9 digits))
                        0
                        $ numsBetween sLen eLen
                 else 0

export
solve2025D2P2 : String -> Int
solve2025D2P2 input = foldr (\s, i => (i+) $ sumCopies2 $ toRange s) 0 $ split (== ',') $ trim input
