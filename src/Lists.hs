module Lists where

import Data.Bool (bool)
import Data.Char (isUpper,toUpper)

acrGen::String->String
acrGen s = [x|x<-s, x `elem` ['A'..'Z']]

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
tuples = length [(x,y)|x<-mySqr,y<-myCube,x<50,y<50]

foldBool3::a->a->Bool->a 
foldBool3 x y True = x 
foldBool3 x y False = y

-- map (\x -> if x == 3 then (-x) else (x)) [1..10]
negateTheThrees xs = map (\x -> bool x (-x) (x==3)) xs
negateTheThrees' xs = [x|x<- map (\x -> bool x (-x) (x==3)) xs] 
negateTheThrees'' xs = [bool x (-x) (x==3) | x<- xs ] 

myZip xs ys = [(x,y) | x<-xs, y<-ys]

myZip'::[a] -> [b] -> [(a,b)]
myZip' _ [] = []
myZip' [] _ = []
myZip' (x:xs) (y:ys) = (x,y):myZip' xs ys

myZipWith f _ [] = []
myZipWith f [] _  = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZip'' = myZipWith (,)

justUpper = filter isUpper
capitalize (c:xs) = toUpper c : xs 
capitalizeAll [] = []
capitalizeAll (c:xs) = toUpper c : capitalizeAll xs 
capitalizedFirst (c:xs) = toUpper c
capitalizedFirst' = toUpper . head

myAnd::[Bool]->Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool 
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem::Eq a=>a->[a]->Bool
myElem _ [] = False
myElem a (x:xs) = a==x || myElem a xs

myElem'::Eq a=>a->[a]->Bool
myElem' x xs = myAny (== x) xs

myReverse :: [a] -> [a]
myReverse = foldl rev [] 
  where rev acc n = n:acc

myReverse' :: [a] -> [a]
myReverse' xs = go xs [] 
  where 
    go::[a]->[a]->[a]
    go [] acc = acc
    go (x:xs) acc = go xs (x:acc)

myReverse'' [] = []
myReverse'' (x:xs) =  myReverse'' xs ++ [x]

squish :: [[a]] -> [a]
squish = foldl (++) [] 

squish' :: [[a]] -> [a]
squish' [] = []
squish' (x:xs) = x ++ squish' xs  

squishMap :: (a -> [b]) -> [a] -> [b] 
squishMap f = squish . map f

squishMap' :: (a -> [b]) -> [a] -> [b] 
squishMap' = flip (>>=)

squishAgain :: [[a]] -> [a] 
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a 
myMaximumBy f (x:xs) = foldl g x xs
 where g x y 
        | f x y == GT = x
        | otherwise = y

myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a 
myMaximumBy' f (x:y:[])  
              | f x y == GT = x
              | otherwise = y
myMaximumBy' f (x:y:xs) 
              | f x y == GT = myMaximumBy' f (x:xs)
              | otherwise = myMaximumBy' f (y:xs)
        
myMaximumBy'' :: (a -> a -> Ordering) -> [a] -> a 
myMaximumBy'' f (x:xs) = go f x xs
    where 
      go _ acc [] = acc
      go f acc (x:xs) 
        | f acc x == GT = go f acc xs
        | otherwise = go f x xs
              
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a 
myMinimumBy = undefined

myMaximum :: (Ord a) => [a] -> a 
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a 
myMinimum = undefined