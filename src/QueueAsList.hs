module QueueAsList
  (Queue, newQueue, isEmptyQueue, enqueue, dequeue, front)
where

-- |
-- >>> newQueue
-- Q []
-- >>> isEmptyQueue newQueue
-- True
-- >>> isEmptyQueue $ enqueue 13 newQueue
-- False
-- >>> isEmptyQueue $ dequeue $ enqueue 13 newQueue
-- True
-- >>> foldr enqueue newQueue "abcdef"
-- Q "fedcba"
-- >>> front $ foldr enqueue newQueue "abcdef"
-- 'f'
-- >>> dequeue $ foldr enqueue newQueue "abcdef"
-- Q "edcba"
-- >>> front $ dequeue $ foldr enqueue newQueue "abcdef"
-- 'e'

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
  
