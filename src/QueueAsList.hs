module QueueAsList (Queue, newQueue, isEmptyQueue, enqueue, dequeue, front) where

-- Public
newQueue     :: Queue a
isEmptyQueue :: Queue a -> Bool
enqueue      :: a -> Queue a -> Queue a
dequeue      :: Queue a -> Queue a
front        :: Queue a -> a

-- Private
newtype Queue a = Q [a]

instance (Show a) => Show (Queue a) where
  showsPrec _ (Q xs) str  = showString "Q " (showList xs str)

newQueue = Q []

isEmptyQueue (Q []) = True
isEmptyQueue (Q _) = False

enqueue x (Q q) = Q (q ++ [x])

dequeue (Q []) = error "queue empty"
dequeue (Q (_:xs)) = Q xs

front (Q []) = error "queue empty"
front (Q (x:_)) = x
  
