module WordNumber where

import Data.List (intersperse)

digitToWord::Int -> String
digitToWord n
 | n == 1 = "one"
 | n == 2 = "two"
 | n == 3 = "three"

digits :: Int -> [Int]
digits n = go n []
  where
    go::Int->[Int] -> [Int]
    go n acc
          | n < 10 = n:acc
          | otherwise = go (div n 10 ) (mod n 10:acc)

wordNumber::Int->String
wordNumber = concat . intersperse "-" . map digitToWord . digits

myWords :: String -> [String]
myWords s = mySplitter' s ' '


firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines s = mySplitter' s '\n'

mySplitter :: String -> Char -> [String]
mySplitter s c = reverse $ go s []
  where
    go::String->[String]->[String]
    go [] acc = acc
    go (x:xs) acc
          | x == c = go xs acc
          | x /= c = go withoutChars $  newChars:acc
          where
                withoutChars = dropWhile (/= c) xs
                newChars = x:takeWhile (/= c) xs

mySplitter' :: String -> Char -> [String]
mySplitter' [] c = []
mySplitter' (x:xs) c
  | x == c = mySplitter' xs c
  | otherwise = (x:takeWhile (/= c) xs):mySplitter (dropWhile (/= c) xs) c
