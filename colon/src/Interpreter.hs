module Interpreter (execute, Dictionary) where

import Stack
import Text.Read (readMaybe)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map

type Dictionary = Map.Map String [String]

execute :: String -> State -> Dictionary -> (State, Dictionary)
execute input state dictionary = interpretTokens (words input) state dictionary

interpretTokens :: [String] -> State -> Dictionary -> (State, Dictionary)
interpretTokens [] state dict = (state, dict)
interpretTokens (token:tokens) state dict
    | token == "CR" = interpretTokens tokens (ioAction (putStrLn "") state) dict
    | token == "." = interpretTokens tokens (unaryOp printTop state) dict
    | head token == '.' && last token == '"' =
        let str = init (tail token)
        in interpretTokens tokens (ioAction (putStr str) state) dict
    | token == ":" =
        let (newState, newDict, restTokens) = defineWord tokens state dict
        in interpretTokens restTokens newState newDict
    | otherwise =
        let newState = interpret token state dict
        in interpretTokens tokens newState dict

interpret :: String -> State -> Dictionary -> State
interpret token state dict = case token of
    "+" -> binaryOp (\a b -> push (a + b)) state
    "-" -> binaryOp (\a b -> push (b - a)) state
    "*" -> binaryOp (\a b -> push (a * b)) state
    "/" -> binaryOp (\a b -> push (b `div` a)) state
    "MOD" -> binaryOp (\a b -> push (b `mod` a)) state
    "DUP" -> unaryOp (\n st -> push n (push n st)) state
    "DROP" -> unaryOp (\_ st -> st) state
    "SWAP" -> binaryOp (\a b st -> push a (push b st)) state
    "<" -> binaryOp (\a b -> push (if b < a then -1 else 0)) state
    ">" -> binaryOp (\a b -> push (if b > a then -1 else 0)) state
    "=" -> binaryOp (\a b -> push (if b == a then -1 else 0)) state
    numStr -> case readMaybe numStr of
        Just num -> push num state
        Nothing -> invokeWord token state dict

invokeWord :: String -> State -> Dictionary -> State
invokeWord token state dict =
    case Map.lookup token dict of
        Just body -> fst $ interpretTokens body state dict
        Nothing -> error $ "Undefined word: " ++ token

defineWord :: [String] -> State -> Dictionary -> (State, Dictionary, [String])
defineWord tokens state dict =
    let (name:body) = takeWhile (/= ";") tokens
        newDict = Map.insert name body dict
        restTokens = drop (length body + 1) tokens  -- Пропускаем тело и ";"
    in (state, newDict, restTokens)

binaryOp :: (Int -> Int -> State -> State) -> State -> State
binaryOp op state =
    let (a, state') = pop state
        (b, state'') = pop state'
    in op a b state''

unaryOp :: (Int -> State -> State) -> State -> State
unaryOp op state =
    let (a, state') = pop state
    in op a state'

ioAction :: IO () -> State -> State
ioAction action state = seq (unsafePerformIO action) state

printTop :: Int -> State -> State
printTop n state = ioAction (print n) state
