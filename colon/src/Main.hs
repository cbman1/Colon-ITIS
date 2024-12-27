module Main where

import Stack
import Arithmetic
import Memory
import qualified IO -- Избегаем конфликта с Prelude
import Interpreter (execute, Dictionary)
import qualified Data.Map as Map

main :: IO ()
main = do
    putStrLn "Welcome to the Colon interpreter!"
    repl emptyState Map.empty

repl :: State -> Dictionary -> IO ()
repl state dict = do
    putStr "Colon> "
    input <- getLine
    let (newState, newDict) = execute input state dict
    IO.printStack newState -- Используем IO.printStack для явного вызова
    repl newState newDict
