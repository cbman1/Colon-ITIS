module Arithmetic where

import Stack

-- Сложение
add :: Stack -> Stack
add stack = let (x, stack') = pop stack
                (y, stack'') = pop stack'
            in push (x + y) stack''

-- Вычитание
sub :: Stack -> Stack
sub stack = let (x, stack') = pop stack
                (y, stack'') = pop stack'
            in push (y - x) stack''

-- Умножение
mul :: Stack -> Stack
mul stack = let (x, stack') = pop stack
                (y, stack'') = pop stack'
            in push (x * y) stack''

-- Деление
div' :: Stack -> Stack
div' stack = let (x, stack') = pop stack
                 (y, stack'') = pop stack'
             in push (y `div` x) stack''

-- Остаток от деления
mod' :: Stack -> Stack
mod' stack = let (x, stack') = pop stack
                 (y, stack'') = pop stack'
             in push (y `mod` x) stack''

-- Равенство ( = )
eq :: Stack -> Stack
eq stack = let (x, stack') = pop stack
               (y, stack'') = pop stack'
           in push (if y == x then -1 else 0) stack''

-- Меньше ( < )
lt :: Stack -> Stack
lt stack = let (x, stack') = pop stack
               (y, stack'') = pop stack'
           in push (if y < x then -1 else 0) stack''

-- Больше ( > )
gt :: Stack -> Stack
gt stack = let (x, stack') = pop stack
               (y, stack'') = pop stack'
           in push (if y > x then -1 else 0) stack''

-- Логическое И ( AND )
and' :: Stack -> Stack
and' stack = let (x, stack') = pop stack
                 (y, stack'') = pop stack'
             in push (if y /= 0 && x /= 0 then -1 else 0) stack''

-- Логическое ИЛИ ( OR )
or' :: Stack -> Stack
or' stack = let (x, stack') = pop stack
                (y, stack'') = pop stack'
            in push (if y /= 0 || x /= 0 then -1 else 0) stack''

-- Логическое НЕ ( INVERT )
invert :: Stack -> Stack
invert stack = let (x, stack') = pop stack
               in push (if x == 0 then -1 else 0) stack'