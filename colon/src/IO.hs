module IO (printStack, readInput) where

import Stack (State(..))  -- Теперь State импортируется с конструкторами

printStack :: State -> IO ()
printStack (State st _) = putStrLn $ "Stack: " ++ show st

readInput :: IO Int
readInput = do
    putStrLn "Enter a number:"
    readLn
