module AdjacencyArrayGraph
  (Graph, adjacent, edgeIn, edgesD, edgesU, mkGraph, nodes, weight)
where

import Data.Ix
import Data.Array

adjacent      :: (Ix n, Num w) => (Graph n w) -> n -> [n]
edgeIn        :: (Ix n, Num w) => (Graph n w) -> (n,n) -> Bool
edgesD,edgesU :: (Ix n, Num w) => (Graph n w) -> [(n,n,w)]
mkGraph       :: (Ix n, Num w) => Bool -> (n,n) -> [(n,n,w)] -> (Graph n w)
nodes         :: (Ix n, Num w) => (Graph n w) -> [n]
weight        :: (Ix n, Num w) => n -> n -> (Graph n w) -> w

type Graph n w = Array n [(n,w)]

adjacent g v = map fst (g!v)

edgeIn g (x,y) = elem y (adjacent g x)

edgesD g = [(x,y,w) | x <- nodes g, (y,w) <- g!x]

edgesU g =
  let f (x,y,_) = x < y
      g' = edgesD g
  in
    filter f g'

mkGraph dir bnds es =
  let p = [(x,(y,w)) | (x,y,w) <- es]
      q = [(y,(x,w)) | (x,y,w) <- es, x /= y]
      f = flip (:)
  in
    accumArray f [] bnds (p ++ if dir then [] else q)
    
nodes = indices 

weight x y g = head [c | (a,c) <- g!x, a == y]

graphAL' = mkGraph False (1,5)
  [(1,2,12)
  ,(1,3,34)
  ,(1,5,78)
  ,(2,4,55)
  ,(2,5,32)
  ,(3,4,61)
  ,(3,5,44)
  ,(4,5,93)]
