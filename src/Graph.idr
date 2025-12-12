||| Various graph utilities

module Graph

import Data.Either
import Data.Fin
import Data.SortedMap
import Data.SortedSet
import Data.Vect
import Utils

public export
record Graph a where
  constructor MkGraph
  graphEdges : SortedMap a (SortedSet a)

export
reachableFrom : Ord a => a -> Graph a -> Either String (SortedSet a)
reachableFrom node (MkGraph edges) =
  case lookup node edges of
    Nothing => Left $ "Input node is not in edges."
    _       => go [node] $ singleton node
  where
    go : List a -> SortedSet a -> Either String (SortedSet a)
    go []        seen = Right $ seen
    go (n :: ns) seen = do
      edgesFromN <- maybeToEither "Search node is not in edges." $ lookup n edges
      let (recurseQueue, recurseSeen) =
        foldl (\(oldQueue, oldSeen), node =>
                if contains node oldSeen then (oldQueue, oldSeen) else (node :: oldQueue, insert node oldSeen))
                (ns, seen) edgesFromN
      go recurseQueue recurseSeen

export
||| Keeps nodes in a graph iff it satisfies the predicate
filterGraph : Ord a => (a -> Bool) -> Graph a -> Graph a
filterGraph pred (MkGraph inputEdges) =
  let outputEdges = map (foldl (\prevEdges, n => if pred n then insert n prevEdges else prevEdges) SortedSet.empty) $ filterKeys pred inputEdges
   in MkGraph outputEdges

export
||| Reverses the edges of a graph, returns the adjacency list
reverseEdges : Ord a => Graph a -> SortedMap a (SortedSet a)
reverseEdges (MkGraph edges) = go (kvList edges) $ map (const empty) edges
  where
    go : List (a, (SortedSet a)) -> SortedMap a (SortedSet a) -> SortedMap a (SortedSet a)
    go []                       reversed = reversed
    go ((n, downstreams) :: nss) reversed =
      go nss (foldl insertF reversed downstreams)
      where
        updateF : Maybe (SortedSet a) -> Maybe (SortedSet a)
        updateF Nothing  = Just $ singleton n
        updateF (Just s) = Just $ insert n s
        insertF : SortedMap a (SortedSet a) -> a -> SortedMap a (SortedSet a)
        insertF nodes downstream = update updateF downstream nodes

export
||| Assuming no cycles, returns the topological order
topoSort : Show a => Ord a => Graph a -> List a
topoSort (MkGraph edges) =
  let startPoints = SortedSet.fromList . map fst . filter (null . snd) $ kvList edges
      newEdges = map (`difference` startPoints) $ filterKeys (not . contains' startPoints) edges
   in if null edges
         then []
         else topoSort (MkGraph newEdges) ++ Prelude.toList startPoints

export
||| Assuming no cycles, find the number of paths from source to sink
numPaths : Show a => Ord a => a -> a -> Graph a -> Either String Int
numPaths source sink (MkGraph edges) =
   case (lookup source edges, lookup sink reversedGraph) of
        (Just _, Just _) => go ordering $ singleton source 1
        _                => Left $ "Source or sink not in edges"
  where
    ordering      = topoSort (MkGraph edges)
    reversedGraph = reverseEdges $ MkGraph edges
    go : List a -> SortedMap a Int -> Either String Int
    go []        table = maybeToEither "Sink not in output table" $ lookup sink table
    go (n :: ns) table = if n == source then go ns table else do
      predecessors <- maybeToEither "predecessors don't exist" $ SortedMap.lookup n reversedGraph
      let listPredecessors = Prelude.toList predecessors
      nums <- maybeToEither "prev value doesn't exist" $ sequence $ map (lookup' table) listPredecessors
      go ns $ insert n (sum nums) table
