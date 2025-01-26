module IO where

import Stack

-- Вывод вершины стека
printTop :: Stack -> IO Stack
printTop stack = do
    let (top:_) = stack  -- получаем верхний элемент стека
    print top            -- выводим его
    return stack         -- возвращаем стек (не изменяется)

-- Вывод перевода строки
cr :: Stack -> IO Stack
cr stack = do
    putStrLn ""
    return stack

-- Вывод символа (команда EMIT)
emit :: Stack -> IO Stack
emit stack = do
    let (x, stack') = pop stack
    putChar (toEnum x)
    return stack'

-- Ввод символа (команда KEY)
key :: Stack -> IO Stack
key stack = do
    c <- getChar
    return (push (fromEnum c) stack)