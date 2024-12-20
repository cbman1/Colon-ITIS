module Interpreter (execute, ifElse, loop) where

import Stack
import Arithmetic
import Memory
import Text.Read (readMaybe)

execute :: String -> State -> State
execute input state = interpretTokens (words input) state

interpretTokens :: [String] -> State -> State
interpretTokens [] state = state
interpretTokens (token:tokens) state = interpretTokens tokens (interpret token state)

interpret :: String -> State -> State
interpret token state = case token of
    "+" -> binaryOp (\a b -> push (a + b)) state
    "-" -> binaryOp (\a b -> push (b - a)) state
    "*" -> binaryOp (\a b -> push (a * b)) state
    "/" -> binaryOp (\a b -> push (b `div` a)) state
    "MOD" -> binaryOp (\a b -> push (b `mod` a)) state
    "DUP" -> unaryOp (\n st -> push n (push n st)) state
    "DROP" -> unaryOp (\_ st -> st) state
    "SWAP" -> binaryOp (\a b st -> push a (push b st)) state
    "OVER" -> overOp state
    "ROT" -> rotOp state
    numStr -> case readMaybe numStr of
        Just num -> push num state
        Nothing -> error $ "Invalid input: " ++ numStr

binaryOp :: (Int -> Int -> State -> State) -> State -> State
binaryOp op state =
    let (a, state') = pop state
        (b, state'') = pop state'
    in op a b state''

unaryOp :: (Int -> State -> State) -> State -> State
unaryOp op state =
    let (a, state') = pop state
    in op a state'

overOp :: State -> State
overOp state =
    let (a, state') = pop state
        (b, state'') = pop state'
    in push b (push a (push b state''))

rotOp :: State -> State
rotOp state =
    let (a, state') = pop state
        (b, state'') = pop state'
        (c, state''') = pop state''
    in push b (push c (push a state'''))

ifElse :: [String] -> State -> State
ifElse tokens st =
    let (cond, st') = pop st
        (trueBranch, rest) = break (== "THEN") tokens
        falseBranch = drop 1 $ dropWhile (/= "ELSE") rest
    in if cond /= 0
       then interpretTokens trueBranch st'
       else interpretTokens falseBranch st'

loop :: [String] -> State -> State
loop tokens st =
    let (start, st') = pop st
        (end, st'') = pop st'
        body = takeWhile (/= "LOOP") tokens
    in loop' start end st'' body

loop' :: Int -> Int -> State -> [String] -> State
loop' current end st body
    | current >= end = st
    | otherwise = loop' (current + 1) end (interpretTokens body st) body
