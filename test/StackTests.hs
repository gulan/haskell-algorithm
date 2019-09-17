module StackTests where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Array
import Data.List (nub,sort)

import StackAsList (Stack, push, pop, top, newStack, isEmptyStack)

new = newStack
empty = isEmptyStack

genStack :: Arbitrary a => Gen (Stack a)
genStack = do
  ns <- arbitrary
  return $ listFoldr push new ns
  where listFoldr :: (a -> b -> b) -> b -> [a] -> b
        listFoldr = foldr

instance Arbitrary a => Arbitrary (Stack a) where
  arbitrary = genStack

stackTests = do
  
  describe "StackAsList Unit Tests" $ do
    it "stack top" $ do
      top (push 13 new) `shouldBe` (13 :: Int)
      
  describe "StackAsList Property Tests" $ do
    it "p stack top" $ do
      property $ \n -> top (push n new) == (n :: Int)
      
    it "p stack pop" $ do
      let
        new :: Stack Int 
        new = newStack
      property $ \n -> pop (push n new) == new

