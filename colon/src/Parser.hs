{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Parser (
    parseAndExecute,
    processTokens
) where

import Stack
import Arithmetic
import IO
import Data.Char (isDigit)
import Data.List (isPrefixOf, isSuffixOf)
import Control.Monad (when, foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import Memory
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

--------------------------------------------------------------------------------
-- Вспомогательный флаг проверки комментариев стека (может быть вынесен в настройки):
--------------------------------------------------------------------------------
strictMode :: Bool
strictMode = True
-- True  => при несоответствии комментария ( a b -- c ) выдавать ошибку
-- False => выдавать предупреждение, но всё равно определять слово

--------------------------------------------------------------------------------
-- Точка входа
--------------------------------------------------------------------------------
parseAndExecute :: String -> Stack -> Dictionary -> MemoryMonad (Stack, Dictionary)
parseAndExecute input stack dictionary = do
    let cleanedInput = removeComments input
    executeTokens (words cleanedInput) stack dictionary False

type ExecuteMonad = MemoryMonad

executeTokens :: [String] -> Stack -> Dictionary -> Bool -> ExecuteMonad (Stack, Dictionary)
executeTokens tokens stack dictionary isDefining =
    processTokens tokens stack dictionary isDefining

--------------------------------------------------------------------------------
-- processTokens
--------------------------------------------------------------------------------
processTokens :: [String] -> Stack -> Dictionary -> Bool -> ExecuteMonad (Stack, Dictionary)
processTokens [] stack dictionary _ = return (stack, dictionary)
processTokens (token:tokens) stack dictionary isDefining
    | isDefining =
        if token == ";" then
            executeTokens tokens stack dictionary False
        else
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
            (finalStack, finalDictionary) <- processBeginUntil bodyTokens stack dictionary
            processTokens (tail remainingTokens) finalStack finalDictionary isDefining

    | isNumber token =
        let number = read token :: Int
        in processTokens tokens (push number stack) dictionary isDefining

    | token `elem`
        [ "+", "-", "*", "/", "MOD"
        , "DUP", "DROP", "SWAP", "OVER", "ROT"
        , ".", "CR", "EMIT", "KEY", ".\""
        , "=", "<", ">", "AND", "OR", "INVERT"
        , "IF", "DO", "I", "LOOP"
        , "VARIABLE", "CONSTANT"
        , "!", "@", "?", "+!", "-!", "*!", "/!", "MOD!"
        ] =
        handleCommand token tokens stack dictionary isDefining

    | otherwise =
        case Map.lookup token dictionary of
            Just definition ->
                processTokens (definition ++ tokens) stack dictionary isDefining
            Nothing -> do
                liftIO $ putStrLn $ "Unknown command: " ++ token
                return (stack, dictionary)

--------------------------------------------------------------------------------
-- Определение нового слова
--------------------------------------------------------------------------------
defineWordDefinition :: String -> [String] -> Stack -> Dictionary -> ExecuteMonad (Stack, Dictionary)
defineWordDefinition wordName tokens stack dictionary = do
    let (definition, remaining) = break (== ";") tokens
    if null remaining
       then do
           liftIO $ putStrLn "Ошибка: Ожидалось ';' для завершения определения слова"
           return (stack, dictionary)
       else do
           let (maybeStackComment, defWithoutComment) = extractStackComment definition
           case maybeStackComment of
             Just (inCount, outCount) -> do
               let netResult = computeStackEffect defWithoutComment dictionary
               case netResult of
                 Left errMsg ->
                   if strictMode
                     then do
                       liftIO $ putStrLn $ "Ошибка при проверке: " ++ errMsg
                       return (stack, dictionary)
                     else do
                       liftIO $ putStrLn $ "Предупреждение: " ++ errMsg
                       let newDict = Map.insert wordName definition dictionary
                       liftIO $ putStrLn "ok"
                       processTokens (tail remaining) stack newDict False
                 Right netEffect ->
                   if netEffect == (outCount - inCount)
                     then do
                       let newDict = Map.insert wordName definition dictionary
                       liftIO $ putStrLn "ok"
                       processTokens (tail remaining) stack newDict False
                     else do
                       let msg = "Неверный стек-эффект: объявлен ("
                                 ++ show inCount ++ " -> " ++ show outCount
                                 ++ "), фактический нетто: " ++ show netEffect
                       if strictMode
                         then do
                           liftIO $ putStrLn $ "Ошибка: " ++ msg
                           return (stack, dictionary)
                         else do
                           liftIO $ putStrLn $ "Предупреждение: " ++ msg
                           let newDict = Map.insert wordName definition dictionary
                           liftIO $ putStrLn "ok"
                           processTokens (tail remaining) stack newDict False

             Nothing -> do
               -- Нет комментария, не проверяем стек-эффект
               let newDict = Map.insert wordName definition dictionary
               liftIO $ putStrLn "ok"
               processTokens (tail remaining) stack newDict False

--------------------------------------------------------------------------------
-- Функция, которая находит в определении комментарий "( a b -- c d )"
--------------------------------------------------------------------------------
extractStackComment :: [String] -> (Maybe (Int, Int), [String])
extractStackComment defs =
    case break isStackComment defs of
      (before, []) -> (Nothing, before)
      (before, (comment:after)) ->
          case parseStackComment comment of
             Just (ic, oc) -> (Just (ic, oc), before ++ after)
             Nothing       -> (Nothing, before ++ after)
  where
    isStackComment t =
        "(" `isPrefixOf` t && ")" `isSuffixOf` t && "--" `elem` words t

parseStackComment :: String -> Maybe (Int, Int)
parseStackComment raw =
    let trimmed = dropWhile (== '(') $ takeWhile (/= ')') raw
        parts   = words trimmed
        idx     = elemIndex "--" parts
    in case idx of
         Nothing -> Nothing
         Just i  ->
           let ins  = take i parts
               outs = drop (i+1) parts
           in Just (length ins, length outs)

elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex _ []     = Nothing
elemIndex x (y:ys) = go 0 x (y:ys)
  where
    go _ _ [] = Nothing
    go n x' (z:zs)
      | x' == z   = Just n
      | otherwise = go (n+1) x' zs

--------------------------------------------------------------------------------
-- Упрощённая функция для вычисления "чистого" эффекта на стек
--------------------------------------------------------------------------------
computeStackEffect :: [String] -> Dictionary -> Either String Int
computeStackEffect = go 0
  where
    go net [] _ = Right net
    go net (t:ts) dict =
      case t of
        "+" -> go (net - 1) ts dict
        "-" -> go (net - 1) ts dict
        "*" -> go (net - 1) ts dict
        "/" -> go (net - 1) ts dict
        "MOD"-> go (net - 1) ts dict
        "DUP" -> go (net + 1) ts dict
        "DROP"-> go (net - 1) ts dict
        "SWAP"-> go net ts dict
        "OVER"-> go (net + 1) ts dict
        "ROT" -> go net ts dict

        "."   -> go (net - 1) ts dict
        "CR"  -> go net ts dict
        "EMIT"-> go (net - 1) ts dict
        "KEY" -> go (net + 1) ts dict
        ".\"" -> go net (skipString ts) dict
        "="   -> go (net - 1) ts dict
        "<"   -> go (net - 1) ts dict
        ">"   -> go (net - 1) ts dict
        "AND" -> go (net - 1) ts dict
        "OR"  -> go (net - 1) ts dict
        "INVERT" -> go net ts dict

        "IF"   -> unknown t
        "ELSE" -> unknown t
        "THEN" -> unknown t
        "BEGIN"-> unknown t
        "UNTIL"-> unknown t
        "DO"   -> unknown t
        "LOOP" -> unknown t
        "I"    -> unknown t

        "VARIABLE"-> unknown t
        "CONSTANT"-> unknown t

        "!" -> go (net - 2) ts dict
        "@" -> go net ts dict -- снимаем addr, кладём val => нетто 0 (или -1+1)
        "?" -> go (net - 1) ts dict
        op | op `elem` ["+!", "-!", "*!", "/!", "MOD!"] -> go (net - 2) ts dict

        -- Если это число
        _ | isNumber t -> go (net + 1) ts dict

          -- Если пользовательское слово
          | Just _def <- Map.lookup t dict ->
               unknown t  -- Можно было бы анализировать рекурсивно, но это сложно
          | otherwise -> unknown t
      where
        unknown tok =
          Left $ "Неизвестный или неподдерживаемый для стат. проверки токен: " ++ tok

skipString :: [String] -> [String]
skipString [] = []
skipString (x:xs)
  | "\"" `isSuffixOf` x = xs
  | otherwise           = skipString xs

--------------------------------------------------------------------------------
-- Обработка команд
--------------------------------------------------------------------------------
handleCommand :: String
              -> [String]
              -> Stack
              -> Dictionary
              -> Bool
              -> ExecuteMonad (Stack, Dictionary)
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
        handleVariableOperator token tokens stack dictionary isDefining

    | token == "BEGIN" || token == "UNTIL" =
        return (stack, dictionary)

    | otherwise = do
        case token of
            "+" -> processTokens tokens (add stack) dictionary isDefining
            "-" -> processTokens tokens (sub stack) dictionary isDefining
            "*" -> processTokens tokens (mul stack) dictionary isDefining
            "/" -> processTokens tokens (div' stack) dictionary isDefining
            "MOD" -> processTokens tokens (mod' stack) dictionary isDefining
            "DUP" -> processTokens tokens (dup stack) dictionary isDefining
            "DROP"-> processTokens tokens (snd $ pop stack) dictionary isDefining
            "SWAP"-> processTokens tokens (swap stack) dictionary isDefining
            "OVER"-> processTokens tokens (over stack) dictionary isDefining
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
                        let _thenTokens = dropWhile (/= "THEN") elseThenPart
                        ifThenElse stack ifPart elseTokens dictionary isDefining
                    else ifThen stack ifTokens dictionary isDefining

            "DO" -> do
                case stack of
                    (end:start:restStack) -> do
                        -- Запускаем цикл с диапазоном [start..(end-1)]
                        let bodyTokens = takeWhile (/= "LOOP") tokens
                            remainingTokens = drop (length bodyTokens + 1) tokens
                        (finalStack, finalDictionary) <- processLoop start end bodyTokens restStack dictionary isDefining
                        processTokens remainingTokens finalStack finalDictionary isDefining
                    _ -> do
                        liftIO $ putStrLn "Ошибка: 'DO' требует два значения на стеке (начало и конец цикла)"
                        return (stack, dictionary)

            "I" -> do
                -- Вместо дублирования вершины стека, берём индекс из currentLoopIndex:
                mem <- get
                case currentLoopIndex mem of
                    Nothing -> do
                        liftIO $ putStrLn "Ошибка: 'I' вне контекста цикла"
                        return (stack, dictionary)
                    Just idx -> do
                        -- Кладём idx на стек
                        processTokens tokens (push idx stack) dictionary isDefining

            "LOOP" -> processTokens tokens stack dictionary isDefining

            _ -> do
                liftIO $ putStrLn $ "Unknown command: " ++ token
                return (stack, dictionary)

--------------------------------------------------------------------------------
-- Операторы над переменными
--------------------------------------------------------------------------------
handleVariableOperator
  :: String
  -> [String]
  -> Stack
  -> Dictionary
  -> Bool
  -> ExecuteMonad (Stack, Dictionary)
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
                                "MOD!"-> if value /= 0 then currentVal `mod` value else currentVal
                                _     -> currentVal
                        setResult <- setVariable varAddr newVal
                        case setResult of
                            Left err -> do
                                liftIO $ putStrLn err
                                processTokens tokens restStack dictionary isDefining
                            Right () -> do
                                liftIO $ putStrLn "ok"
                                processTokens tokens restStack dictionary isDefining
            _ -> do
                liftIO $ putStrLn $ "Ошибка: Стек должен содержать адрес и значение для '" ++ token ++ "'"
                return (stack, dictionary)

    | otherwise = return (stack, dictionary)

--------------------------------------------------------------------------------
-- Пустая defineWord (мы всё делаем в defineWordDefinition).
--------------------------------------------------------------------------------
defineWord :: String -> [String] -> Stack -> Dictionary -> ExecuteMonad (Stack, Dictionary)
defineWord _ _ stk dict = return (stk, dict)

--------------------------------------------------------------------------------
-- IF ... THEN
--------------------------------------------------------------------------------
ifThen :: Stack -> [String] -> Dictionary -> Bool -> ExecuteMonad (Stack, Dictionary)
ifThen stack tokens dictionary isDefining = do
    let (condition, stack') = pop stack
    if condition /= 0
        then processTokens tokens stack' dictionary isDefining
        else return (stack', dictionary)

ifThenElse :: Stack -> [String] -> [String] -> Dictionary -> Bool -> ExecuteMonad (Stack, Dictionary)
ifThenElse stack ifTokens elseTokens dictionary isDefining = do
    let (condition, stack') = pop stack
    if condition /= 0
        then processTokens ifTokens stack' dictionary isDefining
        else processTokens elseTokens stack' dictionary isDefining

splitIfTokens :: [String] -> ([String], [String])
splitIfTokens tokens =
    let ifTokens   = takeWhile (/= "THEN") tokens
        thenTokens = dropWhile (/= "THEN") tokens
    in (ifTokens, thenTokens)

--------------------------------------------------------------------------------
-- Циклы BEGIN ... UNTIL
--------------------------------------------------------------------------------
processBeginUntil :: [String] -> Stack -> Dictionary -> ExecuteMonad (Stack, Dictionary)
processBeginUntil bodyTokens stack dictionary = do
    let loopOnce curStack curDict = do
            (newStack, newDictionary) <- processTokens bodyTokens curStack curDict False
            case newStack of
                (condition:rest) ->
                    if condition /= 0
                        then return (rest, newDictionary)
                        else loopOnce rest newDictionary
                [] -> do
                    liftIO $ putStrLn "Ошибка: Стек пуст при проверке условия UNTIL"
                    return (newStack, newDictionary)
    loopOnce stack dictionary

--------------------------------------------------------------------------------
-- Циклы DO ... LOOP
--------------------------------------------------------------------------------
processLoop :: Int -> Int -> [String] -> Stack -> Dictionary -> Bool -> ExecuteMonad (Stack, Dictionary)
processLoop start end tokens stack dictionary isDefining = do
    let indices = if start < end
                  then [start..(end - 1)]
                  else reverse [end..(start - 1)]
    foldM runIteration (stack, dictionary) indices
  where
    runIteration (curStack, curDict) idx = do
        st <- get
        -- Запоминаем индекс цикла в currentLoopIndex:
        put st { currentLoopIndex = Just idx }

        (resStack, resDict) <- processTokens tokens curStack curDict isDefining

        -- После итерации обнуляем индекс, чтобы не мешал следующему коду.
        st' <- get
        put st' { currentLoopIndex = Nothing }

        return (resStack, resDict)

--------------------------------------------------------------------------------
-- Удаление скобочных комментариев
--------------------------------------------------------------------------------
removeComments :: String -> String
removeComments input = go input 0 []
  where
    go [] _ result = reverse result
    go ('(':xs) lvl result = go xs (lvl + 1) result
    go (')':xs) lvl result = go xs (lvl - 1) result
    go (x:xs) lvl result
        | lvl > 0   = go xs lvl result
        | otherwise = go xs lvl (x:result)

--------------------------------------------------------------------------------
-- Проверка, является ли токен числом
--------------------------------------------------------------------------------
isNumber :: String -> Bool
isNumber token
    | null token          = False
    | all isDigit token   = True
    | head token == '-' && not (null (tail token))
      && all isDigit (tail token) = True
    | otherwise           = False

--------------------------------------------------------------------------------
-- Извлечение строки для ."
--------------------------------------------------------------------------------
extractString :: [String] -> (String, [String])
extractString tokens =
    let (strTokens, rest) = break (\t -> "\"" `isSuffixOf` t) tokens
        str = unwords strTokens
        cleanedStr = if not (null rest)
                     then str ++ " " ++ init (head rest)
                     else str
    in (cleanedStr, drop 1 rest)
