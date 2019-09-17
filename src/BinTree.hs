module BinTree
  (BinTree, addTree,buildTree,delTree,emptyTree,inTree,inorder)
where

import Data.List (nub, sort)

addTree   :: (Ord a, Show a) => a -> BinTree a -> BinTree a
buildTree :: (Ord a, Show a) => [a] -> BinTree a
delTree   :: (Ord a, Show a) => a -> BinTree a -> BinTree a
emptyTree :: (Ord a, Show a) => BinTree a
inTree    :: (Ord a, Show a) => a -> BinTree a -> Bool
inorder   :: (Ord a, Show a) => BinTree a -> [a]

data BinTree a = E | N a (BinTree a) (BinTree a)
  deriving Show

addTree v E  = N v E E
addTree v (N w lf rt) | v == w = N v lf rt
                      | v < w = N w (addTree v lf) rt
                      | v > w = N w lf (addTree v rt)

buildTree = buildTree' . nub . sort

buildTree' [] = E
buildTree' xs = N x (buildTree' lf) (buildTree' rt)
  where lf = take n xs
        (x:rt) = drop n xs
        n = (length xs) `div` 2

delTree _ E  = E
delTree v (N w lf E) | v == w = lf
delTree v (N w E rt) | v == w = rt
delTree v' (N v lf rt) | v' < v = N v (delTree v' lf) rt
                       | v' > v = N v lf (delTree v' rt)
                       | v' == v = let k = minTree rt
                                   in N k lf (delTree k rt)
                      
minTree (N v E _) = v
minTree (N _ lf _) = minTree lf

emptyTree = E

inTree _ E   = False
inTree v (N w lf rt) | v == w = True
                     | v < w = inTree v lf
                     | v > w = inTree v rt

inorder E  = []
inorder (N v lf rt) = inorder lf ++ [v] ++ inorder rt
