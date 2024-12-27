module Memory (declareVariable, setVariable, getVariable, declareConstant) where

import Stack

-- Объявление новой переменной с начальным значением 0
declareVariable :: String -> State -> State
declareVariable name (State st vars) = State st ((name, 0) : vars)

-- Установка значения переменной
setVariable :: String -> Int -> State -> State
setVariable name value (State st vars) = State st (map update vars)
  where
    update (n, v) = if n == name then (n, value) else (n, v)

-- Получение значения переменной
getVariable :: String -> State -> Int
getVariable name (State _ vars) =
    case lookup name vars of
        Just val -> val
        Nothing -> error $ "Variable " ++ name ++ " not found"

-- Объявление константы (по сути, такая же логика, как и переменная, но без изменений)
declareConstant :: String -> Int -> State -> State
declareConstant name value (State st vars) = State st ((name, value) : vars)
