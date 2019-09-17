module TreeTests where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.List (nub,sort)

import qualified BinTree as H

treeTests = 
  describe "BinTree" $ do
    let u ns = nub (sort ns) where types = ns :: [Int]
    it "p bintree" $ do
      property $ \ns -> H.inorder (H.buildTree ns) == (u ns)
