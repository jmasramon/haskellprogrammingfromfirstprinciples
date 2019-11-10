module ADTs where

import Data.Char (toUpper)
import  Data.List.Split (splitOn)
import  Data.List (intercalate)

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True 
isSubsequenceOf (x:xs) ys  
  | elem x ys  = isSubsequenceOf xs ys
  | otherwise = False  
   
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map toTouple . words  
  where 
    toTouple w@(x:xs) = (w, (toUpper x):xs)

capitalizeWord :: String -> String
capitalizeWord "" = "" 
capitalizeWord (' ':x:xs) = ' ':(toUpper x):xs
capitalizeWord (x:xs) = (toUpper x):xs

capitalizeParagraph :: String -> String
capitalizeParagraph = intercalate "." . capitalizeAll. splitOn "."
    where capitalizeAll = map capitalizeWord