module Utils

import Data.SortedSet

export
size : SortedSet a -> Nat
size = foldr (\_, prev => prev + 1) 0
