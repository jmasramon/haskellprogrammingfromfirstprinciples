module Folding where

import Data.Time

myAny'::(a->Bool)->[a]->Bool
myAny' f = foldr (\x acc -> f x || acc) False 

-- foldr f z (x:xs)= f x (foldr f z xs)
-- "(1+(2+(3+(4+(5+0)))))" substitute : for f and [] for z
-- [15,14,12,9,5,0]          works with infinite lists but can overflow
-- foldl f acc (x:xs) = foldl f (f acc x) xs
-- "(((((0+1)+2)+3)+4)+5)"   tail recursive
-- [0,1,3,6,10,15]

data DatabaseItem = 
    DbString String 
  | DbNumber Integer 
  | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem] 
theDatabase =
      [ DbDate (UTCTime
        (fromGregorian 1911 5 1)
        (secondsToDiffTime 34123)), 
      DbNumber 9001, 
      DbNumber 4001, 
      DbString "Hello, world!", 
      DbDate (UTCTime
        (fromGregorian 1921 5 1)
        (secondsToDiffTime 34123))
      ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr filt []
  where 
    filt (DbDate t) acc =(t):acc
    filt _ acc = acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr filt []
  where 
    filt (DbNumber num) acc =(num):acc
    filt _ acc = acc

mostRecent :: [DatabaseItem] -> UTCTime 
mostRecent = ((foldr filt zeroDate) . filterDbDate)
  where 
    filt t acc 
      | t > acc = t
      | otherwise = acc
    zeroDate = UTCTime (fromGregorian 0 1 1) (secondsToDiffTime 0)
      
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double 
avgDb = ((foldr filt 0.0). filterDbNumber)
  where 
    filt num 0.0 = (fromInteger num)
    filt num acc = (acc + (fromInteger num))/2

fibs=1:scanl (+) 1 fibs
smallFibs = (take 10) [x| x<-fibs, x<100]

myFact n = 1:scanl (*) 2 [3..] 

stops  = "pbtdkg"
vowels = "aeiou"

svsTuples::[a]->[a]->[(a, a, a)]
svsTuples stops vowels = [(x,y,z)|x<-stops,y<-vowels,z<-stops]

pvsTuples = [(x,y,z)|x<-stops,y<-vowels,z<-stops, x == 'p']

pvsTuples'::String->String->[(Char, Char, Char)]
pvsTuples' stops vowels = onlyP $ svsTuples stops vowels
    where 
      onlyP::[(Char, Char, Char)]->[(Char, Char, Char)]
      onlyP = filter (\(x,_,_) -> x =='p')

-- seekritFunc::Fractional a => String-> a
seekritFunc x =
  (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))

myOr' :: [Bool] -> Bool 
myOr' = foldr (||) False 

myAny'' :: (a -> Bool) -> [a] -> Bool 
myAny'' f  = myOr' . (map f)

myElem''::Eq a=>a->[a]->Bool
myElem'' n = foldr (\a acc -> (a==n)||acc) False

myElem'''::Eq a=>a->[a]->Bool
myElem''' n = any (==n) 

myReverse_ :: [a] -> [a] 
myReverse_ = foldl (flip (:)) []

myMap::(a->b)->[a]->[b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x:acc else acc) []
