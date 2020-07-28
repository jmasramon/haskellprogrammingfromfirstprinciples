module TryThings where

-- f :: s -> (a, s)

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push x xs = ((),x:xs)

stackManip :: Stack -> (Int, Stack)
stackManip s = let
    ((), s') = push 3 s
    (a, s'') = pop s'
    in
        pop s''
