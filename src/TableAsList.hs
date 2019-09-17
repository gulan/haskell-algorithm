module TableAsList
  (Table, new, find, update)
where

newtype Table a b = T [(b,a)] deriving Show

new     :: (Eq b) => [(b,a)] -> Table a b
find    :: (Eq b) => Table a b -> b -> a
update  :: (Eq b) => (b,a) -> Table a b -> Table a b

new = T

find (T []) _  = error "not found"
find (T ((j,v):t)) i
  | (i==j) = v
  | otherwise = find (T t) i

update a (T [])    = T [a]
update a (T (b:r)) =
  let
    (i,_) = a
    (j,_) = b
  in
    if (i==j)
    then T (a:r)
    else
      let T r' = update a (T r)
      in T (b:r')
  
