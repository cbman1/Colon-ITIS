module Main where

import Stack
import Arithmetic
import Memory
import qualified IO -- Избегаем конфликта с Prelude
import Interpreter

main :: IO ()
main = do
    putStrLn "Welcome to the Colon interpreter!"
    repl emptyState

repl :: State -> IO ()
repl state = do
    putStr "Colon> "
    input <- getLine
    let newState = Interpreter.execute input state
    IO.printStack newState -- Используем IO.printStack для явного вызова
    repl newState
