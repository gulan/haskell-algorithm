module PriorityQueueAsList
  (PQueue, newPQ, isEmptyPQ, enPQ, dePQ, frontPQ)
where

import Data.List (insert)

newtype PQueue a = PQ [a] deriving Show

newPQ     :: (Ord a) => PQueue a
isEmptyPQ :: (Ord a) => PQueue a -> Bool
enPQ      :: (Ord a) => a -> PQueue a -> PQueue a
dePQ      :: (Ord a) => PQueue a -> PQueue a
frontPQ   :: (Ord a) => PQueue a -> a

newPQ = PQ []

isEmptyPQ (PQ []) = True
isEmptyPQ _       = False

enPQ x (PQ xs) = PQ (insert x xs)

dePQ (PQ [])     = error "PQueue empty"
dePQ (PQ (_:xs)) = PQ xs

frontPQ (PQ [])    = error "PQueue empty"
frontPQ (PQ (x:_)) = x
