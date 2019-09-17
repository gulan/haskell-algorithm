module TableTests where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified TableAsArray        as E
import qualified TableAsFunction     as F
import qualified TableAsList         as G

tableTests = 
  describe "TableAsArray" $ do
    let st s = zip [(1::Int)..] s
    let catdog = E.new $ st "catdog"
    
    it "find" $ do
      E.find catdog 2 `shouldBe` 'a'
      
    it "no find" $ do
      evaluate (E.find catdog 99) `shouldThrow` anyException

