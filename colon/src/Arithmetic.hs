module Arithmetic (add, subtract', multiply, divide, mod') where

import Stack

add, subtract', multiply, divide, mod' :: State -> State
add st = let (a, st') = pop st
             (b, st'') = pop st'
         in push (a + b) st''

subtract' st = let (a, st') = pop st
                   (b, st'') = pop st'
               in push (b - a) st''

multiply st = let (a, st') = pop st
                  (b, st'') = pop st'
              in push (a * b) st''

divide st = let (a, st') = pop st
                (b, st'') = pop st'
            in push (b `div` a) st''

mod' st = let (a, st') = pop st
              (b, st'') = pop st'
          in push (b `mod` a) st''
