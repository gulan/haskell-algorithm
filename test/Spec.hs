module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified ArrayTests
import qualified HeapTests
import qualified StackTests
import qualified TableTests
import qualified TreeTests

main :: IO ()
main = hspec $ do
  ArrayTests.arrayTests
  HeapTests.heapTests
  StackTests.stackTests
  TableTests.tableTests
  TreeTests.treeTests
