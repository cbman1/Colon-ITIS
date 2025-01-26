module Stack where

type Stack = [Int]
type LoopStack = [(Int, Int)]  -- (начальное значение, конечное значение)

-- Инициализация пустого стека
emptyStack :: Stack
emptyStack = []

-- Инициализация пустого стека циклов
emptyLoopStack :: LoopStack
emptyLoopStack = []

-- Добавление элемента на стек
push :: Int -> Stack -> Stack
push x stack = x : stack

-- Удаление элемента с вершины стека
pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)
pop [] = error "Stack underflow"

-- Дублирование вершины стека
dup :: Stack -> Stack
dup [] = error "Stack underflow"
dup (x:xs) = x : x : xs

-- Обмен двух верхних элементов стека
swap :: Stack -> Stack
swap (x:y:xs) = y : x : xs
swap _ = error "Stack underflow"

-- Копирование второго элемента на вершину стека
over :: Stack -> Stack
over (x:y:xs) = y : x : y : xs
over _ = error "Stack underflow"

-- Вращение трех верхних элементов стека
rot :: Stack -> Stack
rot (x:y:z:xs) = y : z : x : xs
rot _ = error "Stack underflow"