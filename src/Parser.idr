module Parser

import Data.Fin
import Data.List

public export
record Parser a where
  constructor MkParser
  runParser : List Char -> Maybe (a, List Char)

export
Functor Parser where
  map f (MkParser rp) = MkParser $ \input =>
    case rp input of
         Nothing          => Nothing
         Just (val, rest) => Just (f val, rest)

export
Applicative Parser where
  pure x = MkParser $ \input => Just (x, input)
  (MkParser rpf) <*> (MkParser rpx) = MkParser $ \input =>
    case rpf input of
      Nothing                     => Nothing
      Just (f, intermediateState) =>
        case rpx intermediateState of
          Nothing          => Nothing
          Just (x, output) => Just (f x, output)

export
Monad Parser where
  join (MkParser f) = MkParser $ \input =>
    case f input of
      Nothing                          => Nothing
      Just (MkParser f', intermediate) => f' intermediate

export
Alternative Parser where
  empty = MkParser $ const Nothing
  (MkParser p1) <|> (MkParser p2) =
    MkParser $ \input => p1 input <|> p2 input

mutual
  export
  partial
  some : Parser a -> Parser (List a)
  some p = pure (::) <*> p <*> many p

  export
  partial
  many : Parser a -> Parser (List a)
  many p = some p <|> pure []

export
pChar : Char -> Parser Char
pChar char = MkParser $ \input =>
  case input of
    (c :: cs) => if c == char then Just (char, cs) else Nothing
    _         => Nothing

export
pSpan : (Char -> Bool) -> Parser (List Char)
pSpan pred = MkParser $ \input =>
  let (before, after) := span pred input
   in Just (before, after)

export
pInt : Parser Int
pInt = cast . pack <$> pSpan isDigit

export
pFin : (m : Nat) -> Parser (Maybe (Fin m))
pFin m = (\int => integerToFin (cast int) m) <$> pInt

export
unifyParseOutput : {a : Type} -> Parser a -> List Char -> Either String a
unifyParseOutput parser chars =
  case runParser parser chars of
       Nothing                  => Left $ "Error parsing input!"
       Just (v, [])             => Right v
       Just (_, chars@(_ :: _)) =>
         Left $ "Parsing resulted in leftover chars: " ++ pack chars
