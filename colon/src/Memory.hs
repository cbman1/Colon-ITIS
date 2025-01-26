-- Memory.hs
module Memory (
    MemoryState,
    initialMemory,
    declareVariable,
    setVariable,
    getVariable,
    declareConstant,
    addWord
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import GraphicsState -- Новый модуль для состояния графики

type Address = Int
type Memory = Map Address Int
type Dictionary = Map String [String]

type MemoryMonad = StateT MemoryState IO

-- Обновленная структура состояния памяти
type MemoryState = (Memory, Address, Maybe Int, GraphicsState) -- (memory, nextAddress, lastKey, graphicsState)

-- Инициализация памяти с предварительно заданными WIDTH и HEIGHT
initialMemory :: MemoryState
initialMemory = (initialMem, nextAddr, Nothing, GraphicsState (replicate h (replicate w 0)) w h Nothing)
  where
    w = 10 -- WIDTH = 10
    h = 10 -- HEIGHT = 10
    initialMem = Map.fromList [
        (0, w),   -- WIDTH по адресу 0
        (1, h)    -- HEIGHT по адресу 1
        -- GRAPHICS будет управляться через graphicsState
        ]
    nextAddr = 2 -- Следующий доступный адрес после WIDTH и HEIGHT

-- Функция для добавления слова в словарь
addWord :: String -> [String] -> Dictionary -> Dictionary
addWord = Map.insert

-- Объявление переменной: возвращает её адрес
declareVariable :: String -> MemoryMonad (Either String Address)
declareVariable name = do
    (mem, addr, lastKey, graphicsState) <- get
    if Map.member addr mem
        then return $ Left "Ошибка: Адрес уже занят"
        else do
            let mem' = Map.insert addr 0 mem
            put (mem', addr + 1, lastKey, graphicsState)
            return $ Right addr

-- Объявление константы: устанавливает её значение по адресу
declareConstant :: String -> Int -> MemoryMonad (Either String ())
declareConstant name value = do
    (mem, addr, lastKey, graphicsState) <- get
    if Map.member addr mem
        then return $ Left "Ошибка: Адрес уже занят"
        else do
            let mem' = Map.insert addr value mem
            put (mem', addr + 1, lastKey, graphicsState)
            return $ Right ()

-- Установка значения переменной по адресу
setVariable :: Address -> Int -> MemoryMonad (Either String ())
setVariable addr value = do
    (mem, nextAddr, lastKey, graphicsState) <- get
    if Map.member addr mem
        then do
            let mem' = Map.insert addr value mem
            put (mem', nextAddr, lastKey, graphicsState)
            return $ Right ()
        else return $ Left "Ошибка: Переменная не найдена"

-- Получение значения переменной по адресу
getVariable :: Address -> MemoryMonad (Either String Int)
getVariable addr = do
    (mem, _, _, _) <- get
    case Map.lookup addr mem of
        Just val -> return $ Right val
        Nothing  -> return $ Left "О"

