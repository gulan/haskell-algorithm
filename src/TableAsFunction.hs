module TableAsFunction (Table, new, find, update) where

new     :: (Eq b) => [(b,a)] -> Table a b
find    :: (Eq b) => Table a b -> b -> a
update  :: (Eq b) => (b,a) -> Table a b -> Table a b

newtype Table a b = T (b -> a)

instance Show (Table a b) where
  showsPrec _ _ str = showString "<Table>" str

new alist = foldr update (T err) alist
  where err _ = error "not found"

find (T f) i = f i

update (i,x) (T f) = T g
  where g j | j == i    = x
            | otherwise = f j
