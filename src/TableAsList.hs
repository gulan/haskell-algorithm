module TableAsList
  (Table, new, find, update)
where

-- |
-- >>> new []
-- T []
-- >>> new [(2,'C'),(4,'a'),(6,'t')]
-- T [(2,'C'),(4,'a'),(6,'t')]
-- >>> find (new [(2,'C'),(4,'a'),(6,'t')]) 4
-- 'a'
-- >>> update (4,'o') (new [(2,'C'),(4,'a'),(6,'t')])
-- T [(2,'C'),(4,'o'),(6,'t')]
-- >>> :t new
-- new :: Eq b => [(b, a)] -> Table a b
-- >>> :t new [(True, 'X')]
-- new [(True, 'X')] :: Table Char Bool

newtype Table a b = T [(b,a)]
  deriving Show -- TBD: don't reveal representation with `show`

-- instance (Show a, Show b) => Show (Table a b) where
--   showsPrec _ (T []) str = showChar '}' str
--   showsPrec _ (T ((x,y):xs)) str =
--     case xs of
--       []        -> shows x $ sc $ shows y      $ shows (T xs) str
--       otherwise -> shows x $ sc $ shows y $ ss $ shows (T xs) str
--     where sc     = showChar ':'
--           ss     = showString ", "

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
  
td s = new $ zip [1..] s
