module AdjacencyListGraph
  (Graph, adjacent, edgeIn, edgesD, edgesU, mkGraph, nodes, weight)
where

import Data.List

-- data  Direction = Uni | Bi

adjacent      :: Graph -> Int -> [Int]
edgeIn        :: Graph -> (Int,Int) -> Bool
edgesD,edgesU :: Graph -> [(Int, Int, Int)]
mkGraph       :: Bool -> (Int, Int) -> [(Int, Int, Int)] -> Graph
nodes         :: Graph -> [Int]
weight        :: Int -> Int -> Graph -> Int

type Pair   = (Int,Int)
type PList  = [Pair]
type Graph  = [(Int,PList)]

-- Test data:
g' :: Graph
g' = [
  (1, [(2,12),(3,34),(5,78)]),
  (2, [(1,0),(4,55),(5,32)]),
  (3, [(4,61), (5,44)]),
  (4, [(5,93)])]

es' = edgesD g'

-- Implementation
adjacent gr j   = [k | (i,xs) <- gr, i == j, (k,_) <- xs]
nodes           = map fst
edgeIn gr (x,y) = 1 == length [() | (x',y',_) <- edgesD gr, x==x', y==y']
edgesD gr       = [(i,k,w) | (i,xs) <- gr, (k,w) <- xs]
edgesU gr       = [(x,y,w) | (x,y,w) <- edgesD gr, x < y]
weight x y g    = head [w | (i,j,w) <- edgesD g, i==x, j==y]

mkGraph dir _ es =
  let p = pipe es
      q = pipe [(y,x,w) | (x,y,w) <- es, x /= y]
  in
    p ++ case dir of
           False -> []
           True -> q

-- Helpers:
x |> f = f x

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
