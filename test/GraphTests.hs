module GraphTests where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

-- import AdjacencyArrayGraph as A
import AdjacencyListGraph
-- import GraphSearch as S

genAdjListGraph :: Gen Graph
genAdjListGraph = do
  ps <- (arbitrary :: Gen [(Int,Int,Int)])
  return $ mkGraph False (0,0) ps

instance Arbitrary Graph where
  arbitrary = genAdjListGraph
  -- sample' $ (arbitrary :: Gen Graph)

mk ts = mkGraph False (0,0) $ fmap t23 ts
  where t23 (x,y) = (x,y,0)

chain i j = let m = [i..j]
                ps = zip m (tail m)
            in mk ps

ring i j = let ch = chain i j
               es = edgesD ch
               (i',_,_) = head es
               (_,j',_) = last es
               trp = [(j',i',0)]
           in mkGraph False (0,0) (es ++ trp)

count g = length $ edgesD g

propChain :: Int -> Int -> Property
propChain = \i j -> j > i ==> count (chain i j) + 1 == count (ring i j)

{-
adjacent      :: Graph -> Int -> [Int]
edgeIn        :: Graph -> (Int,Int) -> Bool
edgesD        :: Graph -> [(Int, Int, Int)]
edgesU        :: Graph -> [(Int, Int, Int)]
mkGraph       :: Bool -> (Int, Int) -> [(Int, Int, Int)] -> Graph
nodes         :: Graph -> [Int]
weight        :: Int -> Int -> Graph -> Int
-}

graphTests = do

  describe "single node" $ do
    let g = mkGraph False (1,2) [(1,2,10)]
    describe "adjacent" $ do
      it "unknown"    $ do adjacent g 0 `shouldBe` []
      it "good"       $ do adjacent g 1 `shouldBe` [2]
      it "from range" $ do adjacent g 2 `shouldBe` []
      -- nodes g      `shouldBe` [1]
      -- edgesD g     `shouldBe` [(1,2,10)]
      -- edgesU g     `shouldBe` [(1,2,10)]
      -- weight 1 2 g `shouldBe` 10
        
  describe "Adjacency List Graph Tests" $ do
    it "p ring > chain" $ do
      property propChain
