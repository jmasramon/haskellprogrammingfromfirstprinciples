{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineType where -- simple example

import Data.List (sort)


example = 1

data Rocks =
  Rocks String deriving (Eq, Show, Ord)
data Yeah =
  Yeah Bool deriving (Eq, Show, Ord)
data Papu =
  Papu Rocks Yeah deriving (Eq, Show, Ord)

phew = Papu (Rocks "chases") (Yeah True)

truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool 
comparePapus p p' = p > p'

i::Num a => a
i=1

f::RealFrac a => a
f=1.0

freud::Int -> Int 
freud x=x

myX=1::Int 
sigmund :: Int -> Int
sigmund x = myX

jung::[Int] -> Int
jung xs = head (sort xs)

young :: Ord a=>[a]->a
young xs = head (sort xs)

mySort :: [Char] -> [Char] 
mySort = sort

signifier :: [Char] -> Char 
signifier xs = head (mySort xs)

chk::Eq b => (a->b)->a->b->Bool 
chk fab a b = fab a /= b 

arith::Num b=>(a->b)->Integer->a->b 
arith fab i a = (fab a) * (fromInteger i)

mTh :: Num a => a -> a -> a -> a
mTh x y z=x*y*z
mTh' :: Num a => a -> a -> a -> a
mTh' = \x -> \y -> \z -> x*y*z

addOneIfOdd n = case odd n of 
  True -> f n
  False -> n 
  where f = \n -> n+1

addFive = \x -> \y -> (if x>y then y else x)+5

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer
data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser" 
printUser (RegisteredUser (Username name) (AccountNumber acctNum)) = putStrLn $ name ++ " " ++ show acctNum

data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfrica' :: WherePenguinsLive -> Bool
isSouthAfrica' SouthAfrica = True
isSouthAfrica' _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng wpl) = wpl

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p
  | (Peng Antarctica) <- p = True
  | (Peng Galapagos) <- p = True
  | otherwise = False

k (x,y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2)) 
k3 = k (3,True)

ff :: (a,b,c)->(d,e,f)->((a,d),(c,f)) 
ff (a,b,c) (d,e,f) = ((a,d),(c,f))

functionC x y =if (x>y) then x else y
functionC' x y = case (x>y) of 
  True -> x
  False -> y 

ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2' n = case even n of
  True -> n+2
  False -> n

nums x =
    case compare x 0 of
      LT -> -1 
      GT -> 1
      EQ -> 0

dodgy::Num a => a -> a -> a 
dodgy x y= x + y * 10 
oneIsOne::Num a => a -> a
oneIsOne = dodgy 1 
oneIsTwo::Num a => a -> a
oneIsTwo = (flip dodgy) 2

dogYrs :: Integer -> Integer 
dogYrs x
  | x<=0  =0 
  | x<=1  =x*15 
  | x<=2  =x*12 
  | x<=4  =x*8 
  | otherwise =x*6

avgGrade :: (Fractional a, Ord a) => a -> Char 
avgGrade x
  |y>=0.9 ='A' 
  |y>=0.8 ='B' 
  |y>=0.7 ='C' 
  |y>=0.59='D' 
  |y< 0.59='F' 
  where y=x/100

pal xs
  | xs == reverse xs = True 
  | otherwise = False

tensDigit :: Integral a => a -> a 
tensDigit x = d
  where (xLast, _) = x `divMod` 10
        (_, d) = xLast `divMod` 10

tensDigit' =  (`mod` 10) . (`div` 10)

hunsD x = d2
  where d =  x `div` 100
        d2 = d `mod` 10

hunsD' = tensDigit . (`div` 10) 

getLastDigit = (`mod` 10)
removeLastDigit = (`div` 10)

hunsD'' = getLastDigit . removeLastDigit . removeLastDigit

foldBool :: a -> a -> Bool -> a
foldBool x y p = case p of
  True -> x
  False -> y 

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y p 
  | p = x
  | otherwise = y

foldBool3::a->a->Bool->a 
foldBool3 x y True = x 
foldBool3 x y False = y

g::(a->b)->(a,c)->(b,c) 
g f (a,c) = (f a, c)

--id::a->a
--idx=x
roundTrip :: (Show a, Read b) => a -> b 
roundTrip = read . show 

  