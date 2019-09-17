module QueueAsPair
  (Queue, newQueue, isEmptyQueue, enqueue, dequeue, front)
where

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
  
