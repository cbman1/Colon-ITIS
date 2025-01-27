module Arithmetic where

import Stack

-- Вспомогательные функции конвертации
-- Если хотя бы один из аргументов - F, приводим оба к Double и возвращаем F.
-- Если оба - I, делаем целую операцию и возвращаем I.

addVal :: Value -> Value -> Value
addVal (I x) (I y) = I (x + y)
addVal a b         = F (toD a + toD b)

subVal :: Value -> Value -> Value
subVal (I x) (I y) = I (x - y)
subVal a b         = F (toD a - toD b)

mulVal :: Value -> Value -> Value
mulVal (I x) (I y) = I (x * y)
mulVal a b         = F (toD a * toD b)

divVal :: Value -> Value -> Value
divVal (I x) (I y)
    | y == 0      = error "Division by zero"
    | x `mod` y == 0 = I (x `div` y)  -- целочисленный если делится без остатка
    | otherwise       = F (fromIntegral x / fromIntegral y)
divVal a b =
    let db = toD b
    in if db == 0.0
       then error "Division by zero"
       else F (toD a / db)

modVal :: Value -> Value -> Value
modVal (I x) (I y)
    | y == 0 = error "Division by zero in mod"
    | otherwise = I (x `mod` y)
modVal a b =
    -- Для вещественных чисел: использовать Haskell `mod'` (floating remainder).
    let db = toD b
    in if db == 0.0
       then error "Division by zero in mod"
       else F (toD a `fmod` db)
  where
    -- fmod аналог realFrac, можно использовать Prelude mod' (Data.Fixed) или реализовать вручную
    fmod x y = x - (fromIntegral (floor (x / y)) * y)

--------------------------------------------------------------------------------
-- Основные операции над стеком
--------------------------------------------------------------------------------

add :: Stack -> Stack
add st =
    let (x, st1) = pop st
        (y, st2) = pop st1
    in push (addVal y x) st2

sub :: Stack -> Stack
sub st =
    let (x, st1) = pop st
        (y, st2) = pop st1
    in push (subVal y x) st2

mul :: Stack -> Stack
mul st =
    let (x, st1) = pop st
        (y, st2) = pop st1
    in push (mulVal y x) st2

div' :: Stack -> Stack
div' st =
    let (x, st1) = pop st
        (y, st2) = pop st1
    in push (divVal y x) st2

mod' :: Stack -> Stack
mod' st =
    let (x, st1) = pop st
        (y, st2) = pop st1
    in push (modVal y x) st2

--------------------------------------------------------------------------------
-- Сравнение: возвращаем I (-1) или I 0, как в Forth
--------------------------------------------------------------------------------

eq :: Stack -> Stack
eq st =
    let (x, st1) = pop st
        (y, st2) = pop st1
    in push (if toD y == toD x then I (-1) else I 0) st2

lt :: Stack -> Stack
lt st =
    let (x, st1) = pop st
        (y, st2) = pop st1
    in push (if toD y < toD x then I (-1) else I 0) st2

gt :: Stack -> Stack
gt st =
    let (x, st1) = pop st
        (y, st2) = pop st1
    in push (if toD y > toD x then I (-1) else I 0) st2

--------------------------------------------------------------------------------
-- Логические операции (так же через Int -1/0)
--------------------------------------------------------------------------------

and' :: Stack -> Stack
and' st =
    let (x, st1) = pop st
        (y, st2) = pop st1
        bx = toBool x
        by = toBool y
    in push (if by && bx then I (-1) else I 0) st2

or' :: Stack -> Stack
or' st =
    let (x, st1) = pop st
        (y, st2) = pop st1
        bx = toBool x
        by = toBool y
    in push (if by || bx then I (-1) else I 0) st2

invert :: Stack -> Stack
invert st =
    let (x, st1) = pop st
        b = toBool x
    in push (if b then I 0 else I (-1)) st1

--------------------------------------------------------------------------------
-- Прочие вспомогательные функции
--------------------------------------------------------------------------------

toD :: Value -> Double
toD (I n) = fromIntegral n
toD (F d) = d

toBool :: Value -> Bool
toBool (I 0) = False
toBool (I _) = True
toBool (F f) = (f /= 0.0)