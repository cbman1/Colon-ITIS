-- Main.hs (обновление)
module Main where

import Stack
import Parser
import Memory
import Graphics
import System.IO (hSetEncoding, stdout, stdin, utf8)
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import qualified Data.Map as Map

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8
    putStrLn "Colon Interpreter"
    -- Инициализация состояния памяти и словаря
    -- Объявляем переменную GRAPHICS
    let initialDict = Map.fromList [
            ("WIDTH", ["0"]),
            ("HEIGHT", ["1"]),
            ("GRAPHICS", ["2"]),
            ("LAST-KEY", ["3"])
            ]
    -- Инициализация памяти с объявлением GRAPHICS и LAST-KEY
    let (mem, nextAddr, lastKeyVal) = initialMemory
    -- Инициализируем графическую систему
    graphicsTVar <- initializeGraphics (mem, nextAddr, lastKeyVal)
    let memoryWithGraphics = (mem, nextAddr, lastKeyVal, graphicsTVar)
    -- Запуск главного цикла интерпретатора
    evalStateT (interpreterLoop emptyStack initialDict) memoryWithGraphics
    -- Завершение графической системы
    shutdownGraphics graphicsTVar

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
