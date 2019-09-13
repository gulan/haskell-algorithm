module TableAsArray (Table, new, find, update) where

import Data.Array

newtype Table e i = T (Array i e) deriving Show

new     :: (Ix i) => [(i,e)] -> Table e i
find    :: (Ix i) => Table e i -> i -> e
update  :: (Ix i) => (i,e) -> Table e i -> Table e i

new m =
  let
    inx = map fst m
    lo = minimum inx
    hi = maximum inx
  in
    T (array (lo,hi) m)

find (T a) i  = a ! i

update p (T a) = T (a // [p])
  
