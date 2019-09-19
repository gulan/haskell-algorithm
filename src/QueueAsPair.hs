module QueueAsPair
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

newQueue     :: Queue a
isEmptyQueue :: Queue a -> Bool
enqueue      :: a -> Queue a -> Queue a
dequeue      :: Queue a -> Queue a
front        :: Queue a -> a

newtype Queue a = Q ([a], [a])

instance (Show a) => Show (Queue a) where
  showsPrec _ (Q (front_,rear)) str  =
    let
      p = front_ ++ reverse rear
    in
      showString "Q " $ showList p str

newQueue = Q ([],[])

isEmptyQueue (Q ([],[])) = True
isEmptyQueue (Q _) = False

enqueue x (Q ([],[])) = Q ([x],[])
enqueue y (Q (xs,ys)) = Q (xs,y:ys)

dequeue (Q ([],[])) = error "queue empty"
dequeue (Q ([],ys)) = Q (tail (reverse ys), [])
dequeue (Q (x:xs, ys)) = Q (xs,ys)

front (Q ([],[])) = error "queue empty"
front (Q ([],ys)) = last ys
front (Q (x:_,_)) = x
  
