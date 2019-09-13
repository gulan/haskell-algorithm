module StackAsList (Stack, push, pop, top, newStack, isEmptyStack) where

newtype Stack a = S [a]

instance (Show a) => Show (Stack a) where
  showsPrec _ (S []) str = showChar '-' str
  showsPrec _ (S (x:xs )) str = shows x (showChar '|' (shows (S xs) str))

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

