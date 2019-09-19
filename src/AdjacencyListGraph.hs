module AdjacencyListGraph
  (Graph, adjacent, edgeIn, edgesD, edgesU, mkGraph, nodes, weight)
where

import Data.Function
import Data.List

-- data  Direction = Uni | Bi
-- TBD: the bounds is an artifact of the array impl.; loose it.

adjacent      :: Graph -> Int -> [Int]
edgeIn        :: Graph -> (Int,Int) -> Bool
edgesD,edgesU :: Graph -> [(Int, Int, Int)]
mkGraph       :: Bool -> (Int, Int) -> [(Int, Int, Int)] -> Graph
nodes         :: Graph -> [Int]
weight        :: Int -> Int -> Graph -> Int

type Pair      = (Int,Int)
type PList     = [Pair]
newtype Graph  = G [(Int,PList)] deriving Show

-- |
-- >>> adjacent (fromPairs [(1,2),(1,3),(1,4),(2,2),(2,4),(2,5)]) 1
-- [2,3,4]
-- >>> adjacent (fromPairs [(1,2),(1,3),(1,4),(2,2),(2,4),(2,5)]) 2
-- [2,4,5]
-- >>> adjacent (fromPairs [(1,2),(1,3),(1,4),(2,2),(2,4),(2,5)]) 3
-- []
-- >>> adjacent (fromPairs [(1,2),(1,3),(1,4)]) 1
-- [2,3,4]
-- >>> adjacent (fromPairs [(1,2),(1,3),(1,4)]) 2
-- []
-- >>> adjacent (fromPairs [(1,2)]) 1
-- [2]
-- >>> adjacent (fromPairs []) 1
-- []
adjacent (G ns) j = [k | (i,xs) <- ns, i == j, (k,_) <- xs]

nodes (G ns)      = map fst ns
edgeIn g (x,y)    = 1 == length [() | (x',y',_) <- edgesD g, x==x', y==y']
edgesD (G ns)     = [(i,k,w) | (i,xs) <- ns, (k,w) <- xs]
edgesU g          = [(x,y,w) | (x,y,w) <- edgesD g, x < y]
weight x y g      = head [w | (i,j,w) <- edgesD g, i==x, j==y]

mkGraph dir _ es =
  let p = pipe es
      q = case dir of
        False -> []
        True  -> pipe [(y,x,w) | (x,y,w) <- es, x /= y]
  in
    G (p ++ q)

-- Helpers:
x |> f = f x

-- |
-- >>> fromPairs [(1,2),(1,3),(1,4),(2,2),(2,4),(2,5)]
-- G [(1,[(2,0),(3,0),(4,0)]),(2,[(2,0),(4,0),(5,0)])]
-- >>> toPairs $ fromPairs [(1,2),(1,3),(1,4),(2,2),(2,4),(2,5)]
-- [(1,2),(1,3),(1,4),(2,2),(2,4),(2,5)]
-- >>> toPairs $ fromPairs []
-- []
-- >>> toPairs $ fromPairs [(1,1)]
-- [(1,1)]
fromPairs :: [(Int, Int)] -> Graph
fromPairs pairs = let pl = [(x,y,0) | (x,y) <- pairs]
                  in mkGraph False (0,0) pl

toPairs :: Graph -> [(Int, Int)]
toPairs (G g) = [(x,y) | (x,ys) <- g, (y,_) <- ys] 
                     
structure :: [(Int,Int,Int)] -> [(Int,PList)]
structure = fmap f
  where
    f :: (Int,Int,Int) -> (Int,PList)
    f (x,y,z) =  (x,[(y,z)])

group' = groupBy g
  where g x y = fst x == fst y

ungroup :: [[(Int,PList)]] -> [(Int,PList)]
ungroup = fmap (foldr1 binop)
  where binop (x,ys) (_, ys') = (x, ys ++ ys')

-- |
-- >>> pipe []
-- []
-- >>> pipe [(1,2,3)]
-- [(1,[(2,3)])]
-- >>> pipe [(1,2,0),(1,3,0),(2,1,0)]
-- [(1,[(2,0),(3,0)]),(2,[(1,0)])]
pipe :: [(Int,Int,Int)] -> [(Int,PList)]
pipe es =
  es                      --  [(Int, Int, Int)]
  |> structure            --   [(Int, PList)]
  |> group'               --  [[(Int, PList)]]
  |> ungroup              --   [(Int, PList)]

{-
Nested lists and tuple make structures hard to read. Regex syntax is nicer:
(a,a,a)*
(a,(a,a)*)*  
((a,(a,a)*)*)*
(a,(a,a)*)*  
-}
