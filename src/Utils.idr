module Utils

import Data.SortedMap
import Data.SortedSet

namespace Sets
  export
  size : SortedSet a -> Nat
  size = foldr (\_, prev => prev + 1) 0

namespace Maps
  export
  size : SortedMap a b -> Nat
  size = foldr (\_, prev => prev + 1) 0

  export
  filterKeys : Ord a => (a -> Bool) -> SortedMap a b -> SortedMap a b
  filterKeys pred sm = fromList $ filter (pred . fst) $ kvList sm

export
toIndexedMap : List a -> SortedMap Int a
toIndexedMap =
  Builtin.fst
    . foldl
        (\(prevMap, idx), val => (insert idx val prevMap, idx + 1))
        (empty, 0)
