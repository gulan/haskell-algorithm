module StackAsList
  (Stack, push, pop, top, newStack, isEmptyStack)
where

-- |
-- >>> isEmptyStack newStack
-- True
-- >>> top $ push 11 $ newStack
-- 11
-- >>> isEmptyStack $ pop $ push 11 $ newStack
-- True
-- >>> foldr push newStack "abcdef"
-- 'a'|'b'|'c'|'d'|'e'|'f'|-
-- >>> pop $ foldr push newStack "abcdef"
-- 'b'|'c'|'d'|'e'|'f'|-
-- >>> top $ pop $ foldr push newStack "abcdef"
-- 'b'
-- >>> push (push 13 newStack) newStack
-- 13|-|-
-- >>> pop $ push (push 13 newStack) newStack
-- -
-- >>> top $ push (push 13 newStack) newStack
-- 13|-
-- >>> top $ top $ push (push 13 newStack) newStack
-- 13

newtype Stack a = S [a]

instance (Show a) => Show (Stack a) where
  showsPrec _ (S []) str = showChar '-' str
  showsPrec _ (S (x:xs )) str = shows x (showChar '|' (shows (S xs) str))
  
instance (Eq a) => Eq (Stack a) where
  (S xs) == (S ys) = xs == ys
  
push         :: a -> Stack a -> Stack a
pop          :: Stack a -> Stack a
top          :: Stack a -> a
newStack     :: Stack a
isEmptyStack :: Stack a -> Bool

push x (S sk)  = S (x:sk)

pop (S [])     = error "stack empty"
pop (S (x:xs)) = S xs

top (S [])     = error "stack empty"
top (S (x:xs)) = x

newStack = S []

isEmptyStack (S []) = True
isEmptyStack _  = False

