module AGT where

import Data.Char (ord, isLower, toLower)
import Data.Map (Map, fromList, foldrWithKey)
import Data.List (elemIndex, sort, group, maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (compare)

data Weekday =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

f Friday = "Miller Time"

-- using a combination of chr, ord, and mod again
vigenere::String->String->String
vigenere text keyword = reverse (go text pairs "")
  where
    go::String->[(Char, Char)] -> String -> String
    go [] _ acc = acc
    go (' ':xs) ys acc = go xs ys (' ':acc)
    go (x:xs) (y:ys) acc = go xs ys ((shift y):acc)
    textNoSpaces = filter (/= ' ') text
    tLength = length textNoSpaces
    extendedKeyword = take tLength (concat (repeat keyword))
    pairs = zip textNoSpaces extendedKeyword
    alphabet = ['a'..'z']
    alphLen = length alphabet
    shift::(Char,Char) -> Char
    shift (x,y) = shifted x (shiftSpan y)
    shiftSpan::Char->Int
    shiftSpan c = ord c - ord 'a'
    shifted::Char->Int->Char
    shifted c n = alphabet !! mod (shiftSpan c + n) alphLen

-- validButtons = "1234567890*#"
type Digit = Char
-- Valid presses: 1 and up
type Presses = Int
type Values = [Char]

data PhoneKey = PhoneKey{key::Digit, values::[Char]}
data DaPhone = DaPhone (Map Digit Values) deriving (Show)

phone = DaPhone (fromList [
  ('1',"1"),
  ('2', "2abc"),
  ('3', "3def"),
  ('4', "4ghi"),
  ('5', "5jkl"),
  ('6', "6mno"),
  ('7', "7pqrs"),
  ('8', "8tuv"),
  ('9', "9wxyz"),
  ('*', "^"),
  ('0', "0+_"),
  ('#', ".,")
  ])

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol lol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Haha thanks just making sure rofl ur turn"]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone m) key = foldrWithKey f [] m
  where f k vs acc
                  | elem lowerKey vs = if isLower key
                                  then (k, (fromJust $ elemIndex key vs)+1):acc
                                  else ('*', 1):(k, (fromJust $ elemIndex lowerKey vs)+1):acc
                  | otherwise = acc
        lowerKey = toLower key

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concat . map f
                    where
                      f ' ' = [('0', 2)]
                      f c = reverseTaps phone c

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, p) acc -> acc+p) 0

numberOfRepeatedElems = map (\t@(c:ss) -> (c, length t))

listElemsByFreq::Ord a=>[a] -> [(a, Int)]
listElemsByFreq = numberOfRepeatedElems . group . sort

maximumTupleBySnd::Ord b=>[(a,b)] -> (a,b)
maximumTupleBySnd = maximumBy (\(_, r) (_, r') -> compare r r')

mostRepeatedListElem::Ord a=>[a] -> (a, Int)
mostRepeatedListElem = maximumTupleBySnd . listElemsByFreq

mostPopularListElem :: Ord a=>[a] -> a
mostPopularListElem = fst . mostRepeatedListElem

repeatedListElem::Ord a=>[[a]] -> [(a, Int)]
repeatedListElem = map mostRepeatedListElem

coolestLtr :: Ord a=>[[a]] -> a
coolestLtr = fst . maximumTupleBySnd . repeatedListElem

allWords:: [String] -> [String]
allWords = concat . map words

coolestWord :: [String] -> String
coolestWord =  mostPopularListElem . allWords


data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2

printExpr :: Expr -> String
printExpr (Lit i)= show i
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2
