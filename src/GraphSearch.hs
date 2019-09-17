module GraphSearch where

import AdjacencyListGraph (Graph, adjacent, edgeIn,
                           edgesD, edgesU, mkGraph, nodes, weight)
import StackAsList (Stack, push, pop, top, newStack, isEmptyStack)
import QueueAsPair (Queue, newQueue, isEmptyQueue, enqueue,
                    dequeue, front)

import Data.Foldable (elem)

-- Keep a list of visited nodes, and don't visit twice
depthFirst :: Int -> Graph -> [Int]
depthFirst start g = reverse $ dfs [start] []
  where
    dfs :: [Int] -> [Int] -> [Int]
    dfs [] vis = vis
    dfs (c:cs) vis | elem c vis = dfs cs vis
                   | otherwise  = dfs (adjacent g c ++ cs) (c:vis)

-- Using a stack
depthFirst'' :: Int -> Graph -> [Int]
depthFirst'' start g = reverse $ dfs (push start newStack) []
  where
    dfs :: Stack Int -> [Int] -> [Int]
    dfs s vis | (isEmptyStack s) = vis
              | elem (top s) vis = dfs (pop s) vis
              | otherwise =
                  let
                    c = top s         :: Int
                    aj = adjacent g c :: [Int]
                  in
                    -- weird in that 'pop s' in not a unit. 
                    dfs (foldr push (pop s) aj) (c:vis)

breadthFirst :: Int -> Graph -> [Int]
breadthFirst start g = reverse $ bfs (enqueue start newQueue) []
  where
    bfs :: Queue Int -> [Int] -> [Int]
    bfs s vis | (isEmptyQueue s) = vis
              | elem (front s) vis = bfs (dequeue s) vis
              | otherwise =
                  let
                    c = front s       :: Int
                    aj = adjacent g c :: [Int]
                  in
                    bfs (foldr enqueue (dequeue s) aj) (c:vis)

-- Repl Test Data:

-- 1 -> 1
-- 1 -> 2
-- 2 -> 3 
-- 2 -> 4
-- 4 -> 1
gr = mkGraph False (1,4) [(1,1,0),(1,2,0),(2,3,0),(2,4,0),(4,1,0)]

-- 1 -> 2
-- 2 -> 3
-- 3 -> 4
-- 4 -> 5
-- 4 -> 6
-- 4 -> 7
gr' = mkGraph False (1,4) [(1,2,0),(2,3,0),(3,4,0),(4,5,0),(4,6,0),(4,7,0)]

-- 1 -> 2
-- 1 -> 3
-- 1 -> 4
-- 2 -> 5
-- 2 -> 6
-- 3 -> 7 -> 8
gr'' = mkGraph False (1,4) [(1,2,0),(1,3,0),(1,4,0),(2,5,0),(2,6,0),(3,7,0),(7,8,0)]
