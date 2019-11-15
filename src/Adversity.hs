module Adversity where

import Data.Maybe

-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String 
notThe "the" = Nothing
notThe s = Just s


-- >>> replaceThe "the cow loves us" -- "a cow loves us"
replaceThe :: String -> String 
replaceThe = unwords . fmap (\x -> if (isNothing . notThe) x then "a" else x) . words

replaceThe' :: String -> String 
replaceThe' s = (unwords . reverse) (go (fmap notThe (words s)) [])
  where 
    -- go::[String] -> String -> String
    go [] acc = acc
    go (Nothing:xs) acc = go xs ("a":acc)
    go (Just w:xs) acc = go xs (w:acc)

-- >>> countTheBeforeVowel "the cow" --0
-- >>> countTheBeforeVowel "the evil cow" --1
countTheBeforeVowel :: String -> Integer 
countTheBeforeVowel s = go (words s) 0
    where 
      go::[String]->Integer->Integer
      go [] acc = acc
      go ("the":(y:ys):xs) acc = 
        if (elem y "aeiou") 
          then go xs acc + 1 
          else go ((y:ys):xs) acc
      go (x:xs) acc = go xs acc

countTheBeforeVowel' :: String -> Integer 
countTheBeforeVowel' s = go (((fmap notThe) . words) s) 0
    where 
      go [] acc = acc
      go (Nothing:(Just (y:ys)):xs) acc = 
        if (elem y "aeiou") 
          then go xs acc + 1 
          else go (Just (y:ys):xs) acc
      go (x:xs) acc = go xs acc

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"
isVowel = flip elem $ vowels

mkWord :: String -> Maybe Word' 
mkWord s = if numVows s > numCons s then Nothing else Just (Word' s)
  where 
    numVows = foldr f 0
    f v acc = if isVowel v then acc+1 else acc
    numCons = foldr g 0
    g v acc = if not (isVowel v) then acc+1 else acc

mkWord' :: String -> Maybe Word' 
mkWord' s = go s (0,0)
    where 
      go "" (v,c) = if v > c then Nothing else Just $ Word' s
      go (x:xs) (v,c) = if isVowel x then go xs (v+1,c) else go xs (v,c+1)

data Nat =
      Zero
    | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer 
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat 
integerToNat i = if i < 0 then Nothing else Just $ posIntegerToNat i
  where 
    posIntegerToNat 0 = Zero
    posIntegerToNat x = Succ (posIntegerToNat (x-1))

isJust' :: Maybe a -> Bool
isJust' Nothing = False
isJust' (Just _) = True

isNothing' :: Maybe a -> Bool
isNothing' Nothing = True
isNothing' _ = False

mayybee::b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x 
mayybee _ f (Just y) = f y 

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe (x:xs) = Just x
listToMaybe [] = Nothing

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) =  [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr extract []
  where 
    extract Nothing acc = acc
    extract (Just x) acc = x:acc

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = undefined


lefts' :: [Either a b] -> [a]
lefts' = undefined

rights' :: [Either a b] -> [b]
rights' = undefined

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = undefined

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' = undefined

either'::(a->c)->(b->c)->Either a b->c
either' = undefined

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' = undefined