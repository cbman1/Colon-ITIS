module Stack where

-- Value может быть целым (I Int) или вещественным (F Double).
data Value
    = I Int
    | F Double
    deriving (Eq, Show)

-- Основной стек: список значений
type Stack = [Value]

emptyStack :: Stack
emptyStack = []

-- Добавление
push :: Value -> Stack -> Stack
push v st = v : st

-- Извлечение
pop :: Stack -> (Value, Stack)
pop (x:xs) = (x, xs)
pop []     = error "Stack underflow"

-- Дублирование
dup :: Stack -> Stack
dup (x:xs) = x : x : xs
dup []     = error "Stack underflow"

swap :: Stack -> Stack
swap (x:y:xs) = y : x : xs
swap _        = error "Stack underflow"

over :: Stack -> Stack
over (x:y:xs) = y : x : y : xs
over _        = error "Stack underflow"

rot :: Stack -> Stack
rot (x:y:z:xs) = y : z : x : xs
rot _          = error "Stack underflow"
