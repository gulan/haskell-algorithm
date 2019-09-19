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

preorder E  = []
preorder (N v lf rt) = [v] ++ preorder lf ++ preorder rt

postorder E  = []
postorder (N v lf rt) = preorder lf ++ preorder rt ++ [v]

-- |
-- >>> addTree 5 $ addTree 3 emptyTree
-- N 3 E (N 5 E E)
-- >>> addTree 7 $ addTree 5 $ addTree 3 emptyTree
-- N 3 E (N 5 E (N 7 E E))
-- >>> addTree 3 $ addTree 5 $ addTree 7 emptyTree
-- N 7 (N 5 (N 3 E E) E) E
-- >>> buildTree [3,5,7]
-- N 5 (N 3 E E) (N 7 E E)
-- >>> buildTree [3,5]
-- N 5 (N 3 E E) E
-- >>> addTree 3 $ addTree 5 emptyTree
-- N 5 (N 3 E E) E
-- >>> buildTree [5,3]
-- N 5 (N 3 E E) E
-- >>> addTree 5 $ addTree 3 emptyTree
-- N 3 E (N 5 E E)
-- >>> buildTree [1,2,3]
-- N 2 (N 1 E E) (N 3 E E)
-- >>> buildTree [3,2,1]
-- N 2 (N 1 E E) (N 3 E E)
-- >>> addTree 1 $ addTree 2 emptyTree
-- N 2 (N 1 E E) E
-- >>> foldr addTree emptyTree [1,2,3]
-- N 3 (N 2 (N 1 E E) E) E
-- >>> foldr addTree emptyTree [1,3,2]
-- N 2 (N 1 E E) (N 3 E E)
-- >>> foldr addTree emptyTree [2,3,1]
-- N 1 E (N 3 (N 2 E E) E)
-- >>> foldr addTree emptyTree [3,2,1]
-- N 1 E (N 2 E (N 3 E E))
-- >>> inorder $ foldr addTree emptyTree [3,2,1]
-- [1,2,3]
-- >>> inorder $ foldr addTree emptyTree [1,2,3]
-- [1,2,3]
-- >>> inorder $ foldr addTree emptyTree [1,2,1]
-- [1,2]
-- >>> inorder $ foldr addTree emptyTree [1,3,2]
-- [1,2,3]
-- >>> import Data.List
-- >>> (length . nub . fmap (inorder . buildTree) . permutations) [1..3]
-- 1
-- >>> (length . nub . permutations) [1..3]
-- 6

-- |
-- >>> inTree 6 $ buildTree [2,4..8]
-- True
-- >>> inTree 2 $ buildTree [2,4..8]
-- True
-- >>> inTree 8 $ buildTree [2,4..8]
-- True
-- >>> inTree 5 $ buildTree [2,4..8]
-- False
-- >>> delTree 2 $ buildTree [2,4..8]
-- N 6 (N 4 E E) (N 8 E E)
-- >>> delTree 8 $ delTree 2 $ buildTree [2,4..8]
-- N 6 (N 4 E E) E
-- >>> delTree 6 $ delTree 8 $ delTree 2 $ buildTree [2,4..8]
-- N 4 E E
-- >>> delTree 4 $ delTree 6 $ delTree 8 $ delTree 2 $ buildTree [2,4..8]
-- E
-- >>> delTree 2 emptyTree 
-- E
