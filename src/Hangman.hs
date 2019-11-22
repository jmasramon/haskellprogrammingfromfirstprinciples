module Hangman where

import Control.Monad (forever) -- [1]
import Data.Char (toLower) -- [2]
import Data.Maybe (isJust) -- [3]
import Data.List (intersperse) -- [4]
import System.Exit (exitSuccess) -- [5]
import System.Random (randomRIO) -- [6]

type WordList = [String]

allWords :: IO WordList
allWords = do
  file <- readFile "data/words"
  return (lines file)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9


gameWords :: IO WordList
gameWords = do
  all <- allWords
  return (filter isGameLength all)
  -- return ((filter (((<) minWordLength) . length)) . (filter (((>) maxWordLength) . length)) $ all)
  where
    isGameLength s = l < maxWordLength && l > minWordLength
      where l = length s

randomWord :: WordList -> IO String
randomWord wl = do
  words <- gameWords
  let l = length words
  i <- randomRIO (0, l-1)
  return (words !! i)

data Puzzle = Puzzle String [Maybe Char] String

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed

renderPuzzleChar::Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing = '*'

puzzle::String -> Puzzle
puzzle s = Puzzle s (replicate (length s) Nothing) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) c = c `elem` w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = c `elem` guessed

guess::Char->Puzzle->Puzzle
guess c (Puzzle word discovered guessed) =
  if c `elem` word
  then Puzzle word newDiscovered newGuessed
  else Puzzle word discovered newGuessed
  where
    newGuessed = c:guessed
    zipped = zip word discovered
    newDiscovered = map f zipped
      where
        f::(Char, Maybe Char)-> Maybe Char
        f (x, y) = if x==c then Just c else y

play p = forever $ do
  print "Please chose letter"
  g <- getChar
  let newP = guess g p
  print newP
  let Puzzle word newDiscovered newGuessed = newP
  if all isJust newDiscovered
  then
    print "You won" >> exitSuccess
  else if length newGuessed > 10
    then print "You loose" >> exitSuccess
    else play newP


palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let cleanedLine = filter (isLetter) . map toLower $ line1
  case (cleanedLine == reverse cleanedLine) of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!" >> exitSuccess
  where isLetter c = c `elem` ['a'..'z']

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                    | AgeTooLow
                    | PersonInvalidUnknown String deriving (Eq, Show)
mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  print "Name?"
  name <- getLine
  print "Age?"
  age <- getLine
  let pers = mkPerson name (read age)
  case pers of
    Left _ -> print pers
    othewise -> print "Yay, good person" >> print pers