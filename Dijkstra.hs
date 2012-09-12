{- This Dijkstra implementation is from Bonsai Code].
 - The code can be found at:
 - http://bonsaicode.wordpress.com/2011/01/04/programming-praxis-dijkstra’s-algorithm/
 -
 - Dependencies:
 - http://hackage.haskell.org/package/utility-ht
-}
module Dijkstra where
import Data.List
import qualified Data.List.Key as K
import Data.Map ((!), fromList, fromListWith, adjust, keys, Map)

-- |Build a mapping from the graph
buildGraph :: Ord a => [(a, a, Float)] -> Map a [(a, Float)]
buildGraph g = fromListWith (++) $ g >>=
               \(a,b,d) -> [(a,[(b,d)]), (b,[(a,d)])]

-- |Find the shortest path through a graph
dijkstra :: Ord a => a -> Map a [(a, Float)] -> Map a (Float, Maybe a)
dijkstra source graph =
    f (fromList [(v, (if v == source then 0 else 1/0, Nothing)) | v <- keys graph]) (keys graph)
    where
        f ds [] = ds
        f ds q  = f (foldr relax ds $ graph ! m) (delete m q)
            where
                  m = K.minimum (fst . (ds !)) q
                  relax (e,d) = adjust (min (fst (ds ! m) + d, Just m)) e

-- | List the nodes
shortestPath :: Ord a => a -> a -> Map a [(a, Float)] -> [a]
shortestPath from to graph = reverse $ f to where
    f x = x : maybe [] f (snd $ dijkstra from graph ! x)
