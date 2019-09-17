module HeapTests where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified Heap as I

heapTests = 
  describe "Heap Tests" $ do
    it "empty" $ do
      I.isHeapEmpty (I.emptyHeap :: I.Heap Int) `shouldBe` True
      
    it "not empty" $ do
      I.isHeapEmpty (I.insHeap 5 I.emptyHeap) `shouldBe` False
