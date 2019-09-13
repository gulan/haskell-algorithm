module PriorityQueueAsHeap
  (PQueue, newPQ, isEmptyPQ, enPQ, dePQ, frontPQ)
where

import Heap

newtype PQueue a = PQ (Heap a) deriving Show

dePQ      :: (Ord a) => PQueue a -> PQueue a
enPQ      :: (Ord a) => a -> PQueue a -> PQueue a
frontPQ   :: (Ord a) => PQueue a -> a
isEmptyPQ :: (Ord a) => PQueue a -> Bool
newPQ     :: (Ord a) => PQueue a

dePQ (PQ h)      = PQ (delHeap h)
enPQ v (PQ h)    = PQ (insHeap v h)
frontPQ (PQ h)   = findHeap h
isEmptyPQ (PQ h) = isHeapEmpty h
newPQ            = PQ emptyHeap
