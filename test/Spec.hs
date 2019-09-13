module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.List (nub,sort)

import qualified PriorityQueueAsList as A
import qualified QueueAsList         as B
import qualified QueueAsPair         as C
import qualified StackAsList         as D
import qualified TableAsArray        as E
import qualified TableAsFunction     as F
import qualified TableAsList         as G
import qualified BinTree             as H
import qualified Heap                as I

main :: IO ()
main = hspec $ do
  describe "TableAsArray" $ do
    let st s = zip [(1::Int)..] s
    let catdog = E.new $ st "catdog"
    
    it "find" $ do
      E.find catdog 2 `shouldBe` 'a'
      
    it "no find" $ do
      evaluate (E.find catdog 99) `shouldThrow` anyException

  describe "StackAsList" $ do
    it "stack top" $ do
      D.top (D.push 13 D.newStack) `shouldBe` (13 :: Int)
      
    it "p stack top" $ do
      property $ \n -> D.top (D.push n D.newStack) == (n :: Int)

  describe "BinTree" $ do
    let u ns = nub (sort ns) where types = ns :: [Int]
    it "p bintree" $ do
      property $ \ns -> H.inorder (H.buildTree ns) == (u ns)

  describe "Heap Tests" $ do
    it "empty" $ do
      I.isHeapEmpty (I.emptyHeap :: I.Heap Int) `shouldBe` True
      
    it "not empty" $ do
      I.isHeapEmpty (I.insHeap 5 I.emptyHeap) `shouldBe` False
