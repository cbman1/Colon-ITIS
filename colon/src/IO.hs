module IO where

import Stack

printTop :: Stack -> IO Stack
printTop st =
    case st of
      (val:_) -> do
        putStrLn (showValue val)
        return st
      [] -> do
        putStrLn "<empty stack>"
        return st

cr :: Stack -> IO Stack
cr st = do
    putStrLn ""
    return st

emit :: Stack -> IO Stack
emit st = do
    case st of
      (I x : xs) -> do
        putChar (toEnum x)
        return st
      _ -> do
        putStrLn "Ошибка: EMIT требует целое значение"
        return st

key :: Stack -> IO Stack
key st = do
    c <- getChar
    return (I (fromEnum c) : st)

-- Для красивого вывода:
showValue :: Value -> String
showValue (I n) = show n
showValue (F d) = show d
