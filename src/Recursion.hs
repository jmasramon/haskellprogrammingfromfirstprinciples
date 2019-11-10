module Recursion where -- simple example

factor :: Integer -> Integer
factor n 
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = n * (factor $ n-1)

tsFactor :: Integer -> Integer
tsFactor n = f' 1 n
    where 
      f'::Integer->Integer->Integer
      f' acc n 
        | n == 0 = acc
        | n == 1 = acc
        | otherwise = (f' (n*acc) (n-1))

fibo :: Int -> Integer
fibo n 
      | n == 0 = 1
      | n == 1 = 1
      | otherwise = fibo (n-2) + fibo (n-1)

div'::Integral a => a -> a -> (a, a)
div' n d = go n d 0
      where go n d counter
              | n < d = (counter, n)
              | otherwise  = go (n - d) d (counter + 1)
    