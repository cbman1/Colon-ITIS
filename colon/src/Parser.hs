-- Parser.hs
module Parser (
    parseAndExecute,
    processTokens
) where

import Stack
import Arithmetic
import IO
import Data.Char (isDigit)
import Data.List (isSuffixOf)
import Control.Monad (when, foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import Memory
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Data.Bits (complement, (.&.), (.|.))
import Graphics

-- Парсинг и выполнение команд
parseAndExecute :: String -> Stack -> Dictionary -> MemoryMonad (Stack, Dictionary)
parseAndExecute input stack dictionary = do
    let cleanedInput = removeComments input
    executeTokens (words cleanedInput) stack dictionary False

type ExecuteMonad = MemoryMonad

executeTokens :: [String] -> Stack -> Dictionary -> Bool -> ExecuteMonad (Stack, Dictionary)
executeTokens tokens stack dictionary isDefining =
    processTokens tokens stack dictionary isDefining

-- Обработка токенов (чисел, операций, пользовательских слов и новых команд)
processTokens :: [String] -> Stack -> Dictionary -> Bool -> ExecuteMonad (Stack, Dictionary)
processTokens [] stack dictionary _ = return (stack, dictionary)
processTokens (token:tokens) stack dictionary isDefining
    | isDefining =
        if token == ";" then
            -- Завершение определения слова
            executeTokens tokens stack dictionary False
        else
            -- Добавление токена в текущую дефиницию
            defineWord token tokens stack dictionary
    | token == ":" = do
        case tokens of
            (wordName:rest) -> defineWordDefinition wordName rest stack dictionary
            _ -> do
                liftIO $ putStrLn "Ошибка: Ожидалось имя нового слова после ':'"
                return (stack, dictionary)
    | token == "BEGIN" = do
        let (bodyTokens, remainingTokens) = break (== "UNTIL") tokens
        if null remainingTokens then do
            liftIO $ putStrLn "Ошибка: Ожидалось 'UNTIL' для завершения цикла"
            return (stack, dictionary)
        else do
            -- Выполнение тела цикла до выполнения условия
            (finalStack, finalDictionary) <- processBeginUntil bodyTokens stack dictionary
            processTokens (tail remainingTokens) finalStack finalDictionary isDefining
    | isNumber token = -- Число
        let number = read token :: Int
        in processTokens tokens (push number stack) dictionary isDefining
    | token `elem` ["+", "-", "*", "/", "MOD", "DUP", "DROP", "SWAP", "OVER", "ROT", ".", "CR", "EMIT", "KEY", ".\"", "=", "<", ">", "AND", "OR", "INVERT", "IF", "DO", "I", "LOOP", "VARIABLE", "CONSTANT", "!", "@", "?", "+!", "-!", "*!", "/!", "MOD!", "GRAPHICS", "WIDTH", "HEIGHT", "LAST-KEY"] =
        -- Обработка существующих команд
        handleCommand token tokens stack dictionary isDefining
    | otherwise = case Map.lookup token dictionary of
        Just definition -> processTokens (definition ++ tokens) stack dictionary isDefining
        Nothing -> do
            liftIO $ putStrLn $ "Unknown command: " ++ token
            return (stack, dictionary)

-- Функция для начала определения нового слова
defineWordDefinition :: String -> [String] -> Stack -> Dictionary -> ExecuteMonad (Stack, Dictionary)
defineWordDefinition wordName tokens stack dictionary = do
    -- Собираем все токены до ';' для определения слова
    let (definition, remaining) = break (== ";") tokens
    if null remaining then do
        liftIO $ putStrLn "Ошибка: Ожидалось ';' для завершения определения слова"
        return (stack, dictionary)
    else do
        -- Добавляем слово в Dictionary
        let newDict = Map.insert wordName definition dictionary
        liftIO $ putStrLn "ok"
        -- Продолжаем обработку оставшихся токенов после ';'
        processTokens (tail remaining) stack newDict False

-- Обработка команд
handleCommand :: String -> [String] -> Stack -> Dictionary -> Bool -> ExecuteMonad (Stack, Dictionary)
handleCommand token tokens stack dictionary isDefining
    | token == "VARIABLE" = do
        case tokens of
            (varName:rest) -> do
                result <- declareVariable varName
                case result of
                    Left err -> do
                        liftIO $ putStrLn err
                        processTokens rest stack dictionary isDefining
                    Right addr -> do
                        let newDict = addWord varName [show addr] dictionary
                        liftIO $ putStrLn "ok"
                        processTokens rest stack newDict isDefining
            _ -> do
                liftIO $ putStrLn "Ошибка: Ожидалось имя переменной после 'VARIABLE'"
                return (stack, dictionary)

    | token == "CONSTANT" = do
        case tokens of
            (valueStr:name:rest) ->
                if isNumber valueStr
                    then do
                        let value = read valueStr :: Int
                        result <- declareConstant name value
                        case result of
                            Left err -> do
                                liftIO $ putStrLn err
                                processTokens rest stack dictionary isDefining
                            Right () -> do
                                let newDict = addWord name [show value] dictionary
                                liftIO $ putStrLn "ok"
                                processTokens rest stack newDict isDefining
                    else do
                        liftIO $ putStrLn "Ошибка: Ожидалось числовое значение для константы"
                        return (stack, dictionary)
            _ -> do
                liftIO $ putStrLn "Ошибка: Ожидалось имя и значение константы после 'CONSTANT'"
                return (stack, dictionary)
    | token `elem` ["!", "@", "?", "+!", "-!", "*!", "/!", "MOD!"] = do
        -- Обработка операторов над переменными
        handleVariableOperator token tokens stack dictionary isDefining
    | token == "GRAPHICS" = do
        -- Получение адреса GRAPHICS
        case Map.lookup "GRAPHICS" dictionary of
            Just [addrStr] -> do
                let addr = read addrStr :: Int
                -- Установка состояния GRAPHICS (0 или 1)
                case stack of
                    (val:rest) -> do
                        if val == 0 || val == 1
                            then do
                                setResult <- setVariable addr val
                                case setResult of
                                    Left err -> do
                                        liftIO $ putStrLn err
                                        processTokens tokens rest dictionary isDefining
                                    Right () -> do
                                        liftIO $ putStrLn "ok"
                                        -- Обновление графики
                                        (_, _, _, graphicsState) <- get
                                        liftIO $ updateGraphics graphicsState addr val
                                        processTokens tokens rest dictionary isDefining
                            else do
                                liftIO $ putStrLn "Ошибка: GRAPHICS принимает только 0 или 1"
                                return (stack, dictionary)
                    _ -> do
                        liftIO $ putStrLn "Ошибка: Стек должен содержать значение для GRAPHICS"
                        return (stack, dictionary)
            Nothing -> do
                liftIO $ putStrLn "Ошибка: GRAPHICS не объявлена"
                return (stack, dictionary)
    | token == "WIDTH" = do
        -- Получение WIDTH (адрес 0)
        case Map.lookup "WIDTH" dictionary of
            Just [widthStr] -> do
                let widthVal = read widthStr :: Int
                processTokens tokens (push widthVal stack) dictionary isDefining
            Nothing -> do
                liftIO $ putStrLn "Ошибка: WIDTH не объявлена"
                return (stack, dictionary)
    | token == "HEIGHT" = do
        -- Получение HEIGHT (адрес 1)
        case Map.lookup "HEIGHT" dictionary of
            Just [heightStr] -> do
                let heightVal = read heightStr :: Int
                processTokens tokens (push heightVal stack) dictionary isDefining
            Nothing -> do
                liftIO $ putStrLn "Ошибка: HEIGHT не объявлена"
                return (stack, dictionary)
    | token == "LAST-KEY" = do
        -- Получение последнего нажатого ключа
        (_, _, lastKeyVal, _) <- get
        case lastKeyVal of
            Just keyCode -> processTokens tokens (push keyCode stack) dictionary isDefining
            Nothing -> do
                liftIO $ putStrLn "Ошибка: Нет зарегистрированных нажатий клавиш"
                return (stack, dictionary)
    | otherwise = do
        -- Обработка арифметических и других команд
        case token of
            "+" -> processTokens tokens (add stack) dictionary isDefining
            "-" -> processTokens tokens (sub stack) dictionary isDefining
            "*" -> processTokens tokens (mul stack) dictionary isDefining
            "/" -> processTokens tokens (div' stack) dictionary isDefining
            "MOD" -> processTokens tokens (mod' stack) dictionary isDefining
            "DUP" -> processTokens tokens (dup stack) dictionary isDefining
            "DROP" -> processTokens tokens (snd $ pop stack) dictionary isDefining
            "SWAP" -> processTokens tokens (swap stack) dictionary isDefining
            "OVER" -> processTokens tokens (over stack) dictionary isDefining
            "ROT" -> processTokens tokens (rot stack) dictionary isDefining
            "." -> do
                stack' <- liftIO $ printTop stack
                processTokens tokens stack' dictionary isDefining
            "CR" -> do
                stack' <- liftIO $ cr stack
                processTokens tokens stack' dictionary isDefining
            "EMIT" -> do
                stack' <- liftIO $ emit stack
                processTokens tokens stack' dictionary isDefining
            "KEY" -> do
                stack' <- liftIO $ key stack
                processTokens tokens stack' dictionary isDefining
            ".\"" -> do
                let (str, remainingTokens) = extractString tokens
                liftIO $ putStr str
                processTokens remainingTokens stack dictionary isDefining
            "=" -> processTokens tokens (eq stack) dictionary isDefining
            "<" -> processTokens tokens (lt stack) dictionary isDefining
            ">" -> processTokens tokens (gt stack) dictionary isDefining
            "AND" -> processTokens tokens (and' stack) dictionary isDefining
            "OR" -> processTokens tokens (or' stack) dictionary isDefining
            "INVERT" -> processTokens tokens (invert stack) dictionary isDefining
            "IF" -> do
                let (ifTokens, elseThenTokens) = splitIfTokens tokens
                if "ELSE" `elem` ifTokens
                    then do
                        let (ifPart, elseThenPart) = splitAt (length ifTokens - 1) ifTokens
                        let elseTokens = takeWhile (/= "THEN") elseThenPart
                        let thenTokens = dropWhile (/= "THEN") elseThenPart
                        ifThenElse stack ifPart elseTokens dictionary isDefining
                    else ifThen stack ifTokens dictionary isDefining
            "DO" -> do
                case stack of
                    (end:start:restStack) -> do
                        let bodyTokens = takeWhile (/= "LOOP") tokens
                            remainingTokens = drop (length bodyTokens + 1) tokens
                        -- Запускаем обработку цикла
                        (finalStack, finalDictionary) <- processLoop start end bodyTokens restStack dictionary isDefining
                        -- Обрабатываем оставшиеся токены после цикла
                        processTokens remainingTokens finalStack finalDictionary isDefining
                    _ -> do
                        liftIO $ putStrLn "Ошибка: 'DO' требует два значения на стеке (начало и конец цикла)"
                        return (stack, dictionary)
            "I" -> case tokens of
                (nextToken:rest) -> do
                    -- Здесь предполагается, что 'I' используется внутри цикла для доступа к текущему индексу
                    -- Необходимо хранить текущий индекс в LoopStack
                    -- В текущей реализации LoopStack не используется
                    -- Для полноценной реализации потребуется расширение
                    -- Для простоты, просто дублируем верхнее значение стека
                    processTokens tokens (push (head stack) stack) dictionary isDefining
                _ -> do
                    return (stack, dictionary)
            "LOOP" -> do
                -- 'LOOP' уже обработан в команде 'DO', ничего не делаем
                processTokens tokens stack dictionary isDefining
            _ -> do
                liftIO $ putStrLn $ "Unknown command: " ++ token
                return (stack, dictionary)

-- Обработка операторов над переменными
handleVariableOperator :: String -> [String] -> Stack -> Dictionary -> Bool -> ExecuteMonad (Stack, Dictionary)
handleVariableOperator token tokens stack dictionary isDefining
    | token == "!" = do
        case stack of
            (addr:value:restStack) -> do
                result <- setVariable addr value
                case result of
                    Left err -> do
                        liftIO $ putStrLn err
                        processTokens tokens restStack dictionary isDefining
                    Right () -> do
                        liftIO $ putStrLn "ok"
                        -- Обновление графики, если addr относится к GRAPHICS
                        (_, _, _, graphicsState) <- get
                        liftIO $ updateGraphics graphicsState addr value
                        processTokens tokens restStack dictionary isDefining
            _ -> do
                liftIO $ putStrLn "Ошибка: Стек должен содержать адрес и значение для '!'"
                return (stack, dictionary)
    | token == "@" = do
        case stack of
            (addr:restStack) -> do
                result <- getVariable addr
                case result of
                    Left err -> do
                        liftIO $ putStrLn err
                        processTokens tokens restStack dictionary isDefining
                    Right val -> processTokens tokens (push val restStack) dictionary isDefining
            _ -> do
                liftIO $ putStrLn "Ошибка: Стек должен содержать адрес для '@'"
                return (stack, dictionary)
    | token == "?" = do
        -- Определено как @ .
        case stack of
            (addr:restStack) -> do
                result <- getVariable addr
                case result of
                    Left err -> do
                        liftIO $ putStrLn err
                        processTokens tokens restStack dictionary isDefining
                    Right val -> do
                        liftIO $ print val
                        processTokens tokens restStack dictionary isDefining
            _ -> do
                liftIO $ putStrLn "Ошибка: Стек должен содержать адрес для '?'"
                return (stack, dictionary)
    | token `elem` ["+!", "-!", "*!", "/!", "MOD!"] = do
        -- Арифметические операции над переменной
        case stack of
            (varAddr:value:restStack) -> do
                currentValResult <- getVariable varAddr
                case currentValResult of
                    Left err -> do
                        liftIO $ putStrLn err
                        processTokens tokens restStack dictionary isDefining
                    Right currentVal -> do
                        let newVal = case token of
                                "+!"  -> currentVal + value
                                "-!"  -> currentVal - value
                                "*!"  -> currentVal * value
                                "/!"  -> if value /= 0 then currentVal `div` value else currentVal
                                "MOD!" -> if value /= 0 then currentVal `mod` value else currentVal
                                _     -> currentVal
                        setResult <- setVariable varAddr newVal
                        case setResult of
                            Left err -> do
                                liftIO $ putStrLn err
                                processTokens tokens restStack dictionary isDefining
                            Right () -> do
                                liftIO $ putStrLn "ok"
                                -- Обновление графики, если addr относится к GRAPHICS
                                (_, _, _, graphicsState) <- get
                                liftIO $ updateGraphics graphicsState varAddr newVal
                                processTokens tokens restStack dictionary isDefining
            _ -> do
                liftIO $ putStrLn $ "Ошибка: Стек должен содержать адрес и значение для '" ++ token ++ "'"
                return (stack, dictionary)

-- Условный оператор IF ... THEN
ifThen :: Stack -> [String] -> Dictionary -> Bool -> ExecuteMonad (Stack, Dictionary)
ifThen stack tokens dictionary isDefining = do
    let (condition, stack') = pop stack
    if condition /= 0
        then processTokens tokens stack' dictionary isDefining  -- Выполняем ifPart
        else return (stack', dictionary) -- Пропускаем ifPart

-- Условный оператор IF ... ELSE ... THEN
ifThenElse :: Stack -> [String] -> [String] -> Dictionary -> Bool -> ExecuteMonad (Stack, Dictionary)
ifThenElse stack ifTokens elseTokens dictionary isDefining = do
    let (condition, stack') = pop stack
    if condition /= 0
        then processTokens ifTokens stack' dictionary isDefining
        else processTokens elseTokens stack' dictionary isDefining

-- Разделение токенов для IF ... THEN
splitIfTokens :: [String] -> ([String], [String])
splitIfTokens tokens =
    let ifTokens = takeWhile (/= "THEN") tokens
        thenTokens = dropWhile (/= "THEN") tokens
    in (ifTokens, thenTokens)

-- Проверка, является ли токен числом
isNumber :: String -> Bool
isNumber token
    | null token = False
    | all isDigit token = True
    | head token == '-' && not (null (tail token)) && all isDigit (tail token) = True
    | otherwise = False

-- Удаление комментариев из строки
removeComments :: String -> String
removeComments input = go input 0 []
  where
    go [] _ result = reverse result
    go ('(':xs) level result = go xs (level + 1) result
    go (')':xs) level result = go xs (level - 1) result
    go (x:xs) level result
        | level > 0 = go xs level result  -- Внутри комментария, пропускаем символ
        | otherwise = go xs level (x:result)  -- Вне комментария, сохраняем символ

-- Извлечение строки для команды ."
extractString :: [String] -> (String, [String])
extractString tokens =
    let (strTokens, rest) = break (\t -> "\"" `isSuffixOf` t) tokens
        str = unwords strTokens
        cleanedStr = if not (null rest)
                     then str ++ " " ++ init (head rest)  -- Убираем кавычку
                     else str
    in (cleanedStr, drop 1 rest)  -- Убираем токен с кавычкой

-- Циклическая обработка для команды DO...LOOP
processLoop :: Int -> Int -> [String] -> Stack -> Dictionary -> Bool -> ExecuteMonad (Stack, Dictionary)
processLoop start end tokens stack dictionary isDefining = do
    let indices = if start < end then [start..(end-1)] else reverse [end..(start-1)]
    foldM processLoopIteration (stack, dictionary) indices
  where
    processLoopIteration :: (Stack, Dictionary) -> Int -> ExecuteMonad (Stack, Dictionary)
    processLoopIteration (currentStack, currentDictionary) index = do
        -- Предполагаем, что 'I' должен возвращать текущий индекс
        -- В текущей реализации 'I' просто дублирует верхнее значение
        -- Для полноценной реализации потребуется расширение
        -- Для простоты, просто помещаем индекс на стек
        (finalStack, finalDictionary) <- processTokens tokens (push index currentStack) currentDictionary isDefining
        return (finalStack, finalDictionary)

-- Циклическая обработка для команды BEGIN ... UNTIL
processBeginUntil :: [String] -> Stack -> Dictionary -> ExecuteMonad (Stack, Dictionary)
processBeginUntil bodyTokens stack dictionary = do
    let executeOnce currentStack currentDictionary = do
            -- Выполнение тела цикла
            (newStack, newDictionary) <- processTokens bodyTokens currentStack newDictionary False
            -- Проверка условия: верхнее значение стека
            case newStack of
                (condition:restStack) ->
                    if condition /= 0
                        then return (restStack, newDictionary) -- Условие истинно, завершаем цикл
                        else executeOnce restStack newDictionary -- Условие ложно, продолжаем цикл
                [] -> do
                    liftIO $ putStrLn "Ошибка: Стек пуст при проверке условия в UNTIL"
                    return (newStack, newDictionary)
    executeOnce stack dictionary