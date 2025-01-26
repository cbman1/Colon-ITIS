module Memory where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

-- Тип для хранения пользовательских определений
type Dictionary = Map String [String]

-- Тип для хранения переменных и их адресов
type Variables = Map String Int

-- Тип для хранения значений переменных
type VariableValues = Map Int Int

-- Тип для хранения констант и их значений
type Constants = Map String Int

-- Инициализация пустого словаря
emptyDictionary :: Dictionary
emptyDictionary = Map.empty

-- Инициализация пустых словарей
emptyVariables :: Variables
emptyVariables = Map.empty

emptyVariableValues :: VariableValues
emptyVariableValues = Map.empty

emptyConstants :: Constants
emptyConstants = Map.empty

-- Добавление нового слова в словарь
addWord :: String -> [String] -> Dictionary -> Dictionary
addWord name definition dictionary = Map.insert name definition dictionary

-- Получение определения слова
getWord :: String -> Dictionary -> Maybe [String]
getWord name dictionary = Map.lookup name dictionary

-- Добавление переменной
addVariable :: String -> Int -> Variables -> Variables
addVariable name address variables = Map.insert name address variables

-- Получение адреса переменной
getVariableAddress :: String -> Variables -> Maybe Int
getVariableAddress name variables = Map.lookup name variables

-- Добавление константы
addConstant :: String -> Int -> Constants -> Constants
addConstant name value constants = Map.insert name value constants

-- Получение значения константы
getConstantValue :: String -> Constants -> Maybe Int
getConstantValue name constants = Map.lookup name constants

-- Тип состояния памяти
data MemoryState = MemoryState {
    variables :: Variables,
    variableValues :: VariableValues,
    constants :: Constants,
    nextAddress :: Int
} deriving Show

-- Инициализация состояния памяти
initialMemory :: MemoryState
initialMemory = MemoryState {
    variables = emptyVariables,
    variableValues = emptyVariableValues,
    constants = emptyConstants,
    nextAddress = 1000  -- Начальный адрес для переменных
}

-- Монадический контекст для работы с памятью
type MemoryMonad = StateT MemoryState IO

-- Объявление переменной
declareVariable :: String -> MemoryMonad (Either String Int)
declareVariable name = do
    mem <- get
    if Map.member name (variables mem) || Map.member name (constants mem)
        then return $ Left "Ошибка: Имя уже занято"
        else do
            let addr = nextAddress mem
            put mem { variables = addVariable name addr (variables mem),
                      variableValues = Map.insert addr 0 (variableValues mem),
                      nextAddress = addr + 1 }
            return $ Right addr

-- Объявление константы
declareConstant :: String -> Int -> MemoryMonad (Either String ())
declareConstant name value = do
    mem <- get
    if Map.member name (constants mem) || Map.member name (variables mem)
        then return $ Left "Ошибка: Имя уже занято"
        else do
            put mem { constants = addConstant name value (constants mem) }
            return $ Right ()

-- Установка значения переменной
setVariable :: Int -> Int -> MemoryMonad (Either String ())
setVariable addr val = do
    mem <- get
    if Map.member addr (variableValues mem)
        then do
            let newVarValues = Map.insert addr val (variableValues mem)
            put mem { variableValues = newVarValues }
            return $ Right ()
        else return $ Left "Ошибка: Недопустимый адрес переменной"

-- Получение значения переменной
getVariable :: Int -> MemoryMonad (Either String Int)
getVariable addr = do
    mem <- get
    case Map.lookup addr (variableValues mem) of
        Just val -> return $ Right val
        Nothing -> return $ Left "Ошибка: Недопустимый адрес переменной"
