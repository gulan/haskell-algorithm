module ArrayTests where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Array

-- I don't need to re-test the array package. This is here as a
-- practice exercise.

arrayTests = 
  describe "Haskell Array" $ do
    -- (!)     :: Ix i => Array i e -> i -> e
    -- bounds  :: Array i e -> (i, i)
    -- indices :: Ix i => Array i e -> [i]
    -- elems   :: Array i e -> [e]
    -- assocs  :: Ix i => Array i e -> [(i, e)]
    -- (//)    :: Ix i => Array i e -> [(i, e)] -> Array i e
    -- accum   :: Ix i => (e -> a -> e) -> Array i e -> [(i, a)] -> Array i e
    
    let mk m     = array (1,length m) $ zip [1..] m
    let mk' m    = listArray (1,length m) m
    let colorStr = "redyellowgreen"
    let colors   = mk colorStr
    
    it "makes an array" $ do
      elems colors `shouldBe` colorStr
    
    it "replaces elements in  array" $ do
      (elems $ colors // [(1,'R'),(4,'Y'),(10,'G')]) `shouldBe` "RedYellowGreen"
