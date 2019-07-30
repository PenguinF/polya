
import Control.Exception
import Data.List

import Eutherion.Utilities
import Eutherion.Combinatorics
import Eutherion.Polya


-- Example: graphs on 1 or more points.

-- Usage:
-- -- Get the number of generated permutation functions which map an edge to another, for graphs with 4 vertices:
-- > length $ enumPermuteGraphEdgeMappings $ undirectedGraph 0 3
-- 24
-- -- Now find out where the edge between vertices 0 and 1 ends up when the third (index 2) permutation function is applied to it:
-- > ((enumPermuteGraphEdgeMappings (undirectedGraph 0 3)) !! 2) (GraphEdge (GraphVertex 0) (GraphVertex 1))
-- GraphEdge (GraphVertex 1) (GraphVertex 2)
-- -- Show the Cayley table of the Pólya group for graphs with 4 vertices:
-- > cayleyTable $ graphPolyaGroup 4

data UndirectedGraph a = UndirectedGraph a a deriving (Show, Eq)

undirectedGraph :: Ord a => a -> a -> UndirectedGraph a
undirectedGraph min max = assert (min <= max) $ UndirectedGraph min max

data GraphVertex a = GraphVertex a deriving (Show, Eq, Ord)

vertex :: Ord a => UndirectedGraph a -> a -> GraphVertex a
vertex (UndirectedGraph vmin vmax) n = assert (vmin <= n && n <= vmax) $ GraphVertex n

data GraphEdge a = GraphEdge (GraphVertex a) (GraphVertex a) deriving (Show, Eq)

enumGraphVertices :: Enum a => UndirectedGraph a -> [GraphVertex a]
enumGraphVertices (UndirectedGraph vmin vmax) = [GraphVertex x | x <- [vmin..vmax]]

enumUndirectedNonReflexiveGraphEdges :: (Enum a, Eq a) => UndirectedGraph a -> [GraphEdge a]
enumUndirectedNonReflexiveGraphEdges (UndirectedGraph vmin vmax) =
    [GraphEdge (GraphVertex v) (GraphVertex w) | v <- [vmin..vmax], w <- [v..vmax], v /= w]

enumPermuteGraphEdgeMappings :: (Enum a, Ord a) => UndirectedGraph a -> [GraphEdge a -> GraphEdge a]
enumPermuteGraphEdgeMappings (UndirectedGraph vmin vmax) =
    [permuteEdge $ permuteVertex vertexPermutation | vertexPermutation <- permutations $ enumGraphVertices (UndirectedGraph vmin vmax)]
    where
        permuteVertex :: Enum b => [GraphVertex a] -> GraphVertex b -> GraphVertex a
        permuteVertex vs (GraphVertex n) = vs !! (fromEnum n - fromEnum vmin)

        permuteEdge :: Ord a => (GraphVertex b -> GraphVertex a) -> GraphEdge b -> GraphEdge a
        permuteEdge permuteVertex (GraphEdge v w) =
            case (permuteVertex v, permuteVertex w) of
                (v', w') | v' <= w'  -> GraphEdge v' w'
                         | otherwise -> GraphEdge w' v'

graphPolyaGroup :: (Ord a, Enum a, Num a) => a -> PolyaGroup (GraphEdge a)
graphPolyaGroup n =
    makePolyaGroup (enumUndirectedNonReflexiveGraphEdges ug) (enumPermuteGraphEdgeMappings ug)
    where
        ug = undirectedGraph 0 (n - 1)
