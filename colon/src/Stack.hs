module Stack (State(..), emptyState, push, pop, top) where

type Stack = [Int]
type Variables = [(String, Int)]

data State = State { stack :: Stack, variables :: Variables }

emptyState :: State
emptyState = State [] []

push :: Int -> State -> State
push n (State st vars) = State (n : st) vars

pop :: State -> (Int, State)
pop (State (x:xs) vars) = (x, State xs vars)
pop _ = error "Stack underflow"

top :: State -> Int
top (State (x:_) _) = x
top _ = error "Stack underflow"
