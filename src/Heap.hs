module Heap
  (Heap, emptyHeap, isHeapEmpty, findHeap, insHeap, delHeap)
where

emptyHeap   :: (Ord a) => Heap a
isHeapEmpty :: (Ord a) => Heap a -> Bool
findHeap    :: (Ord a) => Heap a -> a
insHeap     :: (Ord a) => a -> Heap a -> Heap a
delHeap     :: (Ord a) => Heap a -> Heap a

type Rank = Int
data Heap a = EmptyHP
            | HP a Rank (Heap a) (Heap a)
  deriving Show

emptyHeap = EmptyHP

isHeapEmpty EmptyHP = True
isHeapEmpty _ = False

findHeap EmptyHP = error "heap empty"
findHeap (HP x _ _ _) = x

insHeap x h = merge (HP x 1 EmptyHP EmptyHP) h

delHeap EmptyHP = error "heap empty"
delHeap (HP _ _ a b) = merge a b

rank :: (Ord a) => Heap a -> Int
rank EmptyHP = 0
rank (HP _ r _ _) = r

makeHP :: (Ord a) => a -> Heap a -> Heap a -> Heap a
makeHP x a b | rank a >= rank b = HP x (rank b + 1) a b
             | otherwise        = HP x (rank a + 1) b a

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge h EmptyHP = h
merge EmptyHP h = h
merge h1@(HP x _ a1 b1) h2@(HP y _ a2 b2)
  | x <= y    = makeHP x a1 (merge b1 h2)
  | otherwise = makeHP y a2 (merge h1 b2)

-- |
-- >>> emptyHeap
-- EmptyHP
-- >>> isHeapEmpty  emptyHeap
-- True
-- >>> insHeap 1 emptyHeap
-- HP 1 1 EmptyHP EmptyHP
-- >>> insHeap 2 $ insHeap 1 emptyHeap
-- HP 1 1 (HP 2 1 EmptyHP EmptyHP) EmptyHP
-- >>> insHeap 1 $ insHeap 2 emptyHeap
-- HP 1 1 (HP 2 1 EmptyHP EmptyHP) EmptyHP
-- >>> insHeap 1 $ insHeap 1 emptyHeap
-- HP 1 1 (HP 1 1 EmptyHP EmptyHP) EmptyHP
-- >>> foldr insHeap emptyHeap [1,2,3]
-- HP 1 1 (HP 2 1 (HP 3 1 EmptyHP EmptyHP) EmptyHP) EmptyHP
-- >>> delHeap $ foldr insHeap emptyHeap [1,2,3]
-- HP 2 1 (HP 3 1 EmptyHP EmptyHP) EmptyHP
-- >>> delHeap $ delHeap $ foldr insHeap emptyHeap [1,2,3]
-- HP 3 1 EmptyHP EmptyHP
-- >>> delHeap $ delHeap $ delHeap $ foldr insHeap emptyHeap [1,2,3]
-- EmptyHP
-- >>> findHeap  $ foldr insHeap emptyHeap [1,2,3]
-- 1
-- >>> findHeap  $ delHeap  $ foldr insHeap emptyHeap [1,2,3]
-- 2
-- >>> findHeap $ delHeap $ delHeap $ foldr insHeap emptyHeap [1,2,3]
-- 3
-- >>> d3 =  foldl (.) id $ replicate 3 delHeap 
-- >>> foldr insHeap emptyHeap [1..5]
-- HP 1 1 (HP 2 1 (HP 3 1 (HP 4 1 (HP 5 1 EmptyHP EmptyHP) EmptyHP) EmptyHP) EmptyHP) EmptyHP
-- >>> d3 $ foldr insHeap emptyHeap [1..5]
-- HP 4 1 (HP 5 1 EmptyHP EmptyHP) EmptyHP

-- |
-- >>> merge emptyHeap emptyHeap 
-- EmptyHP
-- >>> bh m = foldr insHeap emptyHeap m
-- >>> bh [1]
-- HP 1 1 EmptyHP EmptyHP
-- >>> merge (bh [1]) emptyHeap 
-- HP 1 1 EmptyHP EmptyHP
-- >>> merge (bh [1,2]) emptyHeap 
-- HP 1 1 (HP 2 1 EmptyHP EmptyHP) EmptyHP
-- >>> merge (bh [1,2,3]) emptyHeap 
-- HP 1 1 (HP 2 1 (HP 3 1 EmptyHP EmptyHP) EmptyHP) EmptyHP
-- >>> bh [1,2,3]
-- HP 1 1 (HP 2 1 (HP 3 1 EmptyHP EmptyHP) EmptyHP) EmptyHP
-- >>> findHeap $ bh [1,2,3]
-- 1
-- >>> findHeap $ bh [3,2,1]
-- 1
-- >>> findHeap $ bh [3,1,2]
-- 1
-- >>> findHeap $ merge (bh [3,1,2]) emptyHeap 
-- 1
-- >>> findHeap $ merge (bh [3,1]) (bh [2])
-- 1
-- >>> findHeap $ merge (bh [3]) (bh [2,1])
-- 1
-- >>> findHeap $ merge (bh []) (bh [2,1,3])
-- 1
-- >>> findHeap $ delHeap $ merge (bh []) (bh [2,1,3])
-- 2
-- >>> findHeap $ delHeap $ merge (bh [2]) (bh [1,3])
-- 2
