{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Parser (
    parseAndExecute,
    processTokens
) where

import Stack
import Arithmetic
    ( add, sub, mul, div', mod'
    , eq, lt, gt
    , and', or'
    , invert
    )
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
-- Настройка проверки комментариев ( a b -- c d )
--------------------------------------------------------------------------------

strictMode :: Bool
strictMode = True
-- True:  при несоответствии выдавать ошибку
-- False: выдавать предупреждение

--------------------------------------------------------------------------------
-- Основная точка входа
--------------------------------------------------------------------------------

parseAndExecute :: String -> Stack -> Dictionary -> MemoryMonad (Stack, Dictionary)
parseAndExecute input stack dictionary = do
    let cleanedInput = removeComments input
    executeTokens (words cleanedInput) stack dictionary False

type ExecuteMonad = MemoryMonad

executeTokens :: [String] -> Stack -> Dictionary -> Bool -> ExecuteMonad (Stack, Dictionary)
executeTokens tokens stack dictionary isDef =
    processTokens tokens stack dictionary isDef

--------------------------------------------------------------------------------
-- processTokens
--------------------------------------------------------------------------------

processTokens :: [String] -> Stack -> Dictionary -> Bool -> ExecuteMonad (Stack, Dictionary)
processTokens [] st dict _ = return (st, dict)
processTokens (token:tokens) st dict isDefining

    | isDefining =
        if token == ";" then
            executeTokens tokens st dict False
        else
            defineWord token tokens st dict

    | token == ":" = do
        case tokens of
            (wordName:rest) -> defineWordDefinition wordName rest st dict
            _ -> do
                liftIO $ putStrLn "Ошибка: Ожидалось имя нового слова после ':'"
                return (st, dict)

    | token == "BEGIN" = do
        let (bodyTokens, remainingTokens) = break (== "UNTIL") tokens
        if null remainingTokens
           then do
               liftIO $ putStrLn "Ошибка: Ожидалось 'UNTIL' для завершения цикла"
               return (st, dict)
           else do
               (finalStack, finalDict) <- processBeginUntil bodyTokens st dict
               processTokens (tail remainingTokens) finalStack finalDict isDefining

    | isNumber token =
        case parseValue token of
          Just val -> processTokens tokens (push val st) dict isDefining
          Nothing  -> do
            liftIO $ putStrLn $ "Ошибка парсинга числа: " ++ token
            return (st, dict)

    | token `elem`
        [ "+", "-", "*", "/", "MOD"
        , "DUP", "DROP", "SWAP", "OVER", "ROT"
        , ".", "CR", "EMIT", "KEY", ".\""
        , "=", "<", ">", "AND", "OR", "INVERT"
        , "IF", "DO", "I", "LOOP"
        , "VARIABLE", "CONSTANT"
        , "!", "@", "?", "+!", "-!", "*!", "/!", "MOD!"
        , "S>F", "F>S"
        , "CREATE", "CELLS", "ALLOT"
        ] =
        handleCommand token tokens st dict isDefining

    | otherwise =
        case Map.lookup token dict of
            Just definition ->
                processTokens (definition ++ tokens) st dict isDefining
            Nothing -> do
                liftIO $ putStrLn $ "Unknown command: " ++ token
                return (st, dict)

--------------------------------------------------------------------------------
-- Определение нового слова
--------------------------------------------------------------------------------

defineWordDefinition :: String -> [String] -> Stack -> Dictionary -> ExecuteMonad (Stack, Dictionary)
defineWordDefinition wordName tokens st dict = do
    let (definition, remaining) = break (== ";") tokens
    if null remaining
       then do
           liftIO $ putStrLn "Ошибка: Ожидалось ';' для завершения определения слова"
           return (st, dict)
       else do
           -- Проверяем комментарий ( a b -- c d ), если есть
           let (maybeStackComment, defWithoutComment) = extractStackComment definition
           case maybeStackComment of
             Just (inCount, outCount) -> do
               let netResult = computeStackEffect defWithoutComment dict
               case netResult of
                 Left errMsg ->
                   if strictMode
                     then do
                       liftIO $ putStrLn $ "Ошибка при проверке: " ++ errMsg
                       return (st, dict)
                     else do
                       liftIO $ putStrLn $ "Предупреждение: " ++ errMsg
                       let newDict = Map.insert wordName definition dict
                       liftIO $ putStrLn "ok"
                       processTokens (tail remaining) st newDict False
                 Right netEff ->
                   if netEff == (outCount - inCount)
                     then do
                       let newDict = Map.insert wordName definition dict
                       liftIO $ putStrLn "ok"
                       processTokens (tail remaining) st newDict False
                     else do
                       let msg =
                             "Неверный стек-эффект: объявлен ("
                             ++ show inCount ++ " -> " ++ show outCount
                             ++ "), фактический нетто: " ++ show netEff
                       if strictMode
                         then do
                           liftIO $ putStrLn $ "Ошибка: " ++ msg
                           return (st, dict)
                         else do
                           liftIO $ putStrLn $ "Предупреждение: " ++ msg
                           let newDict = Map.insert wordName definition dict
                           liftIO $ putStrLn "ok"
                           processTokens (tail remaining) st newDict False

             Nothing -> do
               let newDict = Map.insert wordName definition dict
               liftIO $ putStrLn "ok"
               processTokens (tail remaining) st newDict False

defineWord :: String -> [String] -> Stack -> Dictionary -> ExecuteMonad (Stack, Dictionary)
defineWord _ _ st dict = return (st, dict)

--------------------------------------------------------------------------------
-- handleCommand
--------------------------------------------------------------------------------

handleCommand
  :: String
  -> [String]
  -> Stack
  -> Dictionary
  -> Bool
  -> ExecuteMonad (Stack, Dictionary)
handleCommand token tokens st dict isDefining

    ----------------------------------------------------
    -- VARIABLE
    ----------------------------------------------------
    | token == "VARIABLE" = do
        case tokens of
            (varName:rest) -> do
                res <- declareVariable varName
                case res of
                    Left err -> do
                        liftIO $ putStrLn err
                        processTokens rest st dict isDefining
                    Right addr -> do
                        let newDict = addWord varName [show addr] dict
                        liftIO $ putStrLn "ok"
                        processTokens rest st newDict isDefining
            _ -> do
                liftIO $ putStrLn "Ошибка: VARIABLE без имени"
                return (st, dict)

    ----------------------------------------------------
    -- CONSTANT
    ----------------------------------------------------
    | token == "CONSTANT" = do
        case tokens of
            (valueStr:name:rest) ->
                case parseValue valueStr of
                  Just (I i) -> do
                      res <- declareConstant name i
                      case res of
                          Left err -> do
                              liftIO $ putStrLn err
                              processTokens rest st dict isDefining
                          Right () -> do
                              let newDict = addWord name [show i] dict
                              liftIO $ putStrLn "ok"
                              processTokens rest st newDict isDefining
                  Just (F d) -> do
                      liftIO $ putStrLn "Предупреждение: CONSTANT с вещественным числом не поддержано"
                      return (st, dict)
                  Nothing -> do
                      liftIO $ putStrLn "Ошибка: ожидалось число для CONSTANT"
                      return (st, dict)
            _ -> do
                liftIO $ putStrLn "Ошибка: Ожидалось имя и значение константы"
                return (st, dict)

    | token == "CREATE" = do
              case tokens of
                  (name:rest) -> do
                      mem <- get
                      let baseAddr = nextAddress mem
                      -- Добавляем имя в словарь как "константу", которая кладёт baseAddr
                      let newDict = addWord name [show baseAddr] dict
                      liftIO $ putStrLn "ok"
                      -- Не меняем nextAddress прямо здесь
                      processTokens rest st newDict isDefining
                  [] -> do
                      liftIO $ putStrLn "Ошибка: CREATE без имени"
                      return (st, dict)

    | token == "CELLS" = do
              case st of
                  (I n : xs) ->
                      let cellSize = 1  -- если хотим, можно 4
                          newVal   = n * cellSize
                      in processTokens tokens (push (I newVal) xs) dict isDefining

                  (F d : xs) ->
                      let cellSize = 1.0
                          newVal   = d * cellSize
                      in processTokens tokens (push (F newVal) xs) dict isDefining

                  _ -> do
                      liftIO $ putStrLn "Ошибка: CELLS требует число на стеке"
                      return (st, dict)

    | token == "ALLOT" = do
              case st of
                  (I n : xs) -> do
                      mem <- get
                      let oldAddr = nextAddress mem
                          newAddr = oldAddr + n
                      let oldMap = variableValues mem
                      let newMap = fillMemory oldMap oldAddr n
                      put mem { nextAddress = newAddr
                              , variableValues = newMap
                              }
                      liftIO $ putStrLn "ok"
                      processTokens tokens xs dict isDefining

                  (F d : xs) -> do
                      mem <- get
                      let n = floor d
                      let oldAddr = nextAddress mem
                          newAddr = oldAddr + n
                      let oldMap = variableValues mem
                      let newMap = fillMemory oldMap oldAddr n
                      put mem { nextAddress = newAddr
                              , variableValues = newMap
                              }
                      liftIO $ putStrLn "ok"
                      processTokens tokens xs dict isDefining

                  _ -> do
                      liftIO $ putStrLn "Ошибка: ALLOT требует число на вершине стека"
                      return (st, dict)

    ----------------------------------------------------
    -- Операторы над переменными
    ----------------------------------------------------
    | token `elem` ["!", "@", "?", "+!", "-!", "*!", "/!", "MOD!"] = do
        handleVariableOperator token tokens st dict isDefining

    ----------------------------------------------------
    -- Преобразования: S>F, F>S
    ----------------------------------------------------
    | token == "S>F" = do
        case st of
          (I i : xs) -> processTokens tokens (push (F (fromIntegral i)) xs) dict isDefining
          (F _ : _)  -> do
            liftIO $ putStrLn "Ошибка: S>F требует целое (I int) на вершине"
            return (st, dict)
          [] -> do
            liftIO $ putStrLn "Ошибка: пустой стек для S>F"
            return (st, dict)

    | token == "F>S" = do
        case st of
          (F d : xs) -> processTokens tokens (push (I (truncate d)) xs) dict isDefining
          (I _ : _)  -> do
            liftIO $ putStrLn "Ошибка: F>S требует вещественное (F double) на вершине"
            return (st, dict)
          [] -> do
            liftIO $ putStrLn "Ошибка: пустой стек для F>S"
            return (st, dict)

    ----------------------------------------------------
    -- BEGIN / UNTIL уже обрабатываются в processTokens
    ----------------------------------------------------
    | token == "BEGIN" || token == "UNTIL" =
        return (st, dict)

    ----------------------------------------------------
    -- Арифметика и прочие
    ----------------------------------------------------
    | otherwise =
        case token of
            "+" -> processTokens tokens (add st) dict isDefining
            "-" -> processTokens tokens (sub st) dict isDefining
            "*" -> processTokens tokens (mul st) dict isDefining
            "/" -> processTokens tokens (div' st) dict isDefining
            "MOD"-> processTokens tokens (mod' st) dict isDefining

            "DUP" -> processTokens tokens (dup st) dict isDefining
            "DROP"-> processTokens tokens (snd $ pop st) dict isDefining
            "SWAP"-> processTokens tokens (swap st) dict isDefining
            "OVER"-> processTokens tokens (over st) dict isDefining
            "ROT" -> processTokens tokens (rot st) dict isDefining

            "." -> do
                st' <- liftIO $ printTop st
                processTokens tokens st' dict isDefining
            "CR" -> do
                st' <- liftIO $ cr st
                processTokens tokens st' dict isDefining
            "EMIT" -> do
                st' <- liftIO $ emit st
                processTokens tokens st' dict isDefining
            "KEY" -> do
                st' <- liftIO $ key st
                processTokens tokens st' dict isDefining
            ".\"" -> do
                let (str, remainingTokens) = extractString tokens
                liftIO $ putStr str
                processTokens remainingTokens st dict isDefining

            "=" -> processTokens tokens (eq st) dict isDefining
            "<" -> processTokens tokens (lt st) dict isDefining
            ">" -> processTokens tokens (gt st) dict isDefining
            "AND" -> processTokens tokens (and' st) dict isDefining
            "OR" -> processTokens tokens (or' st) dict isDefining
            "INVERT" -> processTokens tokens (invert st) dict isDefining

            ------------------------------------------------
            -- IF ... THEN ... ELSE
            ------------------------------------------------
            "IF" -> do
                let (ifTokens, elseThenTokens) = splitIfTokens tokens
                if "ELSE" `elem` ifTokens
                   then do
                       let (ifPart, elseThenPart) = splitAt (length ifTokens - 1) ifTokens
                       let elseTokens = takeWhile (/= "THEN") elseThenPart
                       let _thenTokens = dropWhile (/= "THEN") elseThenPart
                       ifThenElse st ifPart elseTokens dict isDefining
                   else
                       ifThen st ifTokens dict isDefining

            ------------------------------------------------
            -- DO ... LOOP
            ------------------------------------------------
            "DO" -> do
                case st of
                    (I end : I start : restSt) -> do
                        let bodyTokens = takeWhile (/= "LOOP") tokens
                            remainingTokens = drop (length bodyTokens + 1) tokens
                        (finalStack, finalDict) <- processLoop start end bodyTokens restSt dict isDefining
                        processTokens remainingTokens finalStack finalDict isDefining
                    (F _ : _) -> do
                        liftIO $ putStrLn "Ошибка: DO требует целые (I Int) start/end"
                        return (st, dict)
                    _ -> do
                        liftIO $ putStrLn "Ошибка: 'DO' требует два Int значения на стеке"
                        return (st, dict)

            "I" -> do
                mem <- get
                case currentLoopIndex mem of
                    Nothing -> do
                        liftIO $ putStrLn "Ошибка: 'I' вне контекста DO ... LOOP"
                        return (st, dict)
                    Just idx -> do
                        processTokens tokens (push (I idx) st) dict isDefining

            "LOOP" -> processTokens tokens st dict isDefining

            _ -> do
                liftIO $ putStrLn $ "Unknown command: " ++ token
                return (st, dict)

--------------------------------------------------------------------------------
-- IF ... THEN ... ELSE
--------------------------------------------------------------------------------

ifThen :: Stack -> [String] -> Dictionary -> Bool -> ExecuteMonad (Stack, Dictionary)
ifThen st tokens dict isDef = do
    let (cond, restSt) = pop st
    if toBool' cond
       then processTokens tokens restSt dict isDef
       else return (restSt, dict)

ifThenElse :: Stack -> [String] -> [String] -> Dictionary -> Bool -> ExecuteMonad (Stack, Dictionary)
ifThenElse st ifTokens elseTokens dict isDef = do
    let (cond, restSt) = pop st
    if toBool' cond
       then processTokens ifTokens restSt dict isDef
       else processTokens elseTokens restSt dict isDef

splitIfTokens :: [String] -> ([String], [String])
splitIfTokens tokens =
    let ifTokens   = takeWhile (/= "THEN") tokens
        thenTokens = dropWhile (/= "THEN") tokens
    in (ifTokens, thenTokens)

--------------------------------------------------------------------------------
-- BEGIN ... UNTIL
--------------------------------------------------------------------------------

processBeginUntil :: [String] -> Stack -> Dictionary -> ExecuteMonad (Stack, Dictionary)
processBeginUntil bodyTokens st dict = do
    let loopOnce curStack curDict = do
            (newStack, newDict) <- processTokens bodyTokens curStack curDict False
            case newStack of
                (cond:rest) ->
                    if toBool' cond
                        then return (rest, newDict)
                        else loopOnce rest newDict
                [] -> do
                    liftIO $ putStrLn "Ошибка: Стек пуст при проверке UNTIL"
                    return (newStack, newDict)
    loopOnce st dict

--------------------------------------------------------------------------------
-- DO ... LOOP
--------------------------------------------------------------------------------

processLoop :: Int -> Int -> [String] -> Stack -> Dictionary -> Bool -> ExecuteMonad (Stack, Dictionary)
processLoop start end tokens st dict isDef = do
    let indices =
          if start < end
             then [start..end-1]
             else reverse [end..start-1]
    foldM runIteration (st, dict) indices
  where
    runIteration (curSt, curDict) idx = do
        stMem <- get
        put stMem { currentLoopIndex = Just idx }

        (resStack, resDict) <- processTokens tokens curSt curDict isDef

        stMem' <- get
        put stMem' { currentLoopIndex = Nothing }

        return (resStack, resDict)

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
handleVariableOperator token tokens st dict isDef
    | token == "!" = do
        case st of
            (I value : I addr : rest) -> do
                res <- setVariable addr value
                case res of
                    Left err -> do
                        liftIO $ putStrLn err
                        processTokens tokens rest dict isDef
                    Right () -> do
                        liftIO $ putStrLn "ok"
                        processTokens tokens rest dict isDef
            (I _ : F _ : _) -> do
                liftIO $ putStrLn "Ошибка: нельзя записать вещественное в Int-переменную (упрощённый пример)"
                return (st, dict)
            _ -> do
                liftIO $ putStrLn "Ошибка: '!' требует (I addr, I value)"
                return (st, dict)

    | token == "@" = do
        case st of
            (I addr : rest) -> do
                res <- getVariable addr
                case res of
                    Left err -> do
                        liftIO $ putStrLn err
                        processTokens tokens rest dict isDef
                    Right val ->
                        processTokens tokens (push (I val) rest) dict isDef
            _ -> do
                liftIO $ putStrLn "Ошибка: '@' требует Int addr"
                return (st, dict)

    | token == "?" = do
        case st of
            (I addr : rest) -> do
                res <- getVariable addr
                case res of
                    Left err -> do
                        liftIO $ putStrLn err
                        processTokens tokens rest dict isDef
                    Right val -> do
                        liftIO $ print val
                        processTokens tokens rest dict isDef
            _ -> do
                liftIO $ putStrLn "Ошибка: '?' требует Int addr"
                return (st, dict)

    | token `elem` ["+!", "-!", "*!", "/!", "MOD!"] = do
        case st of
            (I varAddr : I value : rest) -> do
                curValRes <- getVariable varAddr
                case curValRes of
                    Left err -> do
                        liftIO $ putStrLn err
                        processTokens tokens rest dict isDef
                    Right curVal -> do
                        let newVal = case token of
                                "+!"  -> curVal + value
                                "-!"  -> curVal - value
                                "*!"  -> curVal * value
                                "/!"  -> if value /= 0 then curVal `div` value else curVal
                                "MOD!"-> if value /= 0 then curVal `mod` value else curVal
                                _     -> curVal
                        setRes <- setVariable varAddr newVal
                        case setRes of
                            Left e -> do
                                liftIO $ putStrLn e
                                processTokens tokens rest dict isDef
                            Right () -> do
                                liftIO $ putStrLn "ok"
                                processTokens tokens rest dict isDef
            (I _ : F _ : _) -> do
                liftIO $ putStrLn "Ошибка: нельзя вещественным менять переменную Int"
                return (st, dict)
            _ -> do
                liftIO $ putStrLn $ "Ошибка: " ++ token ++ " требует (Int addr, Int value)"
                return (st, dict)

    | otherwise = return (st, dict)

--------------------------------------------------------------------------------
-- Помощник для заполнения памяти при ALLOT
--------------------------------------------------------------------------------

fillMemory :: Map Int Int -> Int -> Int -> Map Int Int
fillMemory varMap start count =
    foldl (\acc i -> Map.insert (start + i) 0 acc) varMap [0..(count-1)]

--------------------------------------------------------------------------------
-- Удаление скобочных комментариев ( ... )
--------------------------------------------------------------------------------

removeComments :: String -> String
removeComments input = go input 0 []
  where
    go [] _ acc = reverse acc
    go ('(':xs) lvl acc = go xs (lvl + 1) acc
    go (')':xs) lvl acc = go xs (lvl - 1) acc
    go (x:xs) lvl acc
        | lvl > 0   = go xs lvl acc
        | otherwise = go xs lvl (x:acc)

--------------------------------------------------------------------------------
-- Проверка, является ли строка числом (целым или вещественным)
--------------------------------------------------------------------------------

isNumber :: String -> Bool
isNumber s =
    case parseValue s of
      Just _ -> True
      _      -> False

-- Парсим строку в Value:
-- Сперва пытаемся как Int, если неудачно — Double.
parseValue :: String -> Maybe Value
parseValue s =
    case reads s :: [(Int, String)] of
      [(i, "")] -> Just (I i)
      _         -> case reads s :: [(Double, String)] of
                     [(d, "")] -> Just (F d)
                     _         -> Nothing

--------------------------------------------------------------------------------
-- Преобразование Value в Bool (для IF, UNTIL и т.д.)
--------------------------------------------------------------------------------

-- Переименовано в toBool', чтобы не конфликтовать с Arithmetic.
toBool' :: Value -> Bool
toBool' (I 0) = False
toBool' (I _) = True
toBool' (F f) = (f /= 0.0)

--------------------------------------------------------------------------------
-- Извлечение строки для команды ."
--------------------------------------------------------------------------------

extractString :: [String] -> (String, [String])
extractString tokens =
    let (strTokens, rest) = break (\t -> "\"" `isSuffixOf` t) tokens
        str = unwords strTokens
        cleanedStr =
            if not (null rest)
               then str ++ " " ++ init (head rest)
               else str
    in (cleanedStr, drop 1 rest)

--------------------------------------------------------------------------------
-- Статическая проверка комментария ( a b -- c d )
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
-- Упрощённая статическая проверка нетто-эффекта
--------------------------------------------------------------------------------

computeStackEffect :: [String] -> Dictionary -> Either String Int
computeStackEffect = go 0
  where
    go net [] _ = Right net
    go net (t:ts) dict
      | t == "+"    = go (net - 1) ts dict
      | t == "-"    = go (net - 1) ts dict
      | t == "*"    = go (net - 1) ts dict
      | t == "/"    = go (net - 1) ts dict
      | t == "MOD"  = go (net - 1) ts dict

      | t == "DUP"  = go (net + 1) ts dict
      | t == "DROP" = go (net - 1) ts dict
      | t == "SWAP" = go net ts dict
      | t == "OVER" = go (net + 1) ts dict
      | t == "ROT"  = go net ts dict

      | t == "."    = go (net - 1) ts dict
      | t == "CR"   = go net ts dict
      | t == "EMIT" = go (net - 1) ts dict
      | t == "KEY"  = go (net + 1) ts dict
      | t == ".\""  = go net (skipString ts) dict
      | t == "="    = go (net - 1) ts dict
      | t == "<"    = go (net - 1) ts dict
      | t == ">"    = go (net - 1) ts dict
      | t == "AND"  = go (net - 1) ts dict
      | t == "OR"   = go (net - 1) ts dict
      | t == "INVERT" = go net ts dict

      | t == "IF"    = unknown t
      | t == "ELSE"  = unknown t
      | t == "THEN"  = unknown t
      | t == "BEGIN" = unknown t
      | t == "UNTIL" = unknown t
      | t == "DO"    = unknown t
      | t == "LOOP"  = unknown t
      | t == "I"     = unknown t

      | t == "VARIABLE" = unknown t
      | t == "CONSTANT" = unknown t
      | t == "CREATE"   = unknown t
      | t == "CELLS"    = unknown t
      | t == "ALLOT"    = unknown t


      | t == "!"   = go (net - 2) ts dict
      | t == "@"   = go net ts dict    -- addr -> val (net effect 0)
      | t == "?"   = go (net - 1) ts dict
      | t `elem` ["+!", "-!", "*!", "/!", "MOD!"] = go (net - 2) ts dict

      | t == "S>F" = unknown t
      | t == "F>S" = unknown t

      -- Если это число
      | isNumber t = go (net + 1) ts dict

      -- Может быть пользовательское слово?
      | Just _ <- Map.lookup t dict = unknown t
      | otherwise = Left ("Неизвестный токен: " ++ t)
      where
        unknown x = Left ("Невозможно статически вычислить эффект для: " ++ x)

skipString :: [String] -> [String]
skipString [] = []
skipString (x:xs)
  | "\"" `isSuffixOf` x = xs
  | otherwise           = skipString xs
