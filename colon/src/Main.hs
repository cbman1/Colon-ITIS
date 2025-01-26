module Main where

import Stack
import Parser
import Memory
import System.IO (hSetEncoding, stdout, stdin, utf8)
import Control.Monad.State

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8
    putStrLn "Colon Interpreter"
    -- Инициализация состояния памяти и словаря
    evalStateT (interpreterLoop emptyStack emptyDictionary) initialMemory

-- Главный цикл интерпретатора
interpreterLoop :: Stack -> Dictionary -> MemoryMonad ()
interpreterLoop stack dictionary = do
    liftIO $ putStr "> "
    input <- liftIO getLine
    if input == "quit"
        then return ()
        else do
            -- Выполнение команды
            (newStack, newDictionary) <- parseAndExecute input stack dictionary
            -- Переход на новую строку и вывод состояния стека
            liftIO $ putStrLn ""
            liftIO $ putStrLn $ "Stack: " ++ showStack newStack
            -- Переход к следующей итерации
            interpreterLoop newStack newDictionary

-- Функция для отображения состояния стека (вершина справа)
showStack :: Stack -> String
showStack [] = "<empty>"
showStack stack = unwords (map show (reverse stack))
