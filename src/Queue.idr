module Queue

import Data.List

export
record Queue a where
  constructor MkQueue
  forward  : List a
  backward : List a

export
Show a => Show (Queue a) where
  show (MkQueue f b) = show $ f ++ reverse b

export
singleton : a -> Queue a
singleton x = MkQueue [] [x]

export
null : Queue a -> Bool
null (MkQueue f b) = null f && null b

export
push : a -> Queue a -> Queue a
push val (MkQueue f b) = MkQueue (val :: f) b

export
pop : Queue a -> Maybe (a, Queue a)
pop (MkQueue [] [])       = Nothing
pop (MkQueue f (b :: bs)) = Just (b, MkQueue f bs)
pop (MkQueue f [])        =
  case reverse f of
    (b :: bs) => Just (b, MkQueue [] bs)
    _         => Nothing
