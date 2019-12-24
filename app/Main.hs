module Main where

import qualified Data.Text as T
import System.IO
import ADTs
import Adversity
import AGT
import Aplicative
import BinaryTree
import Cipher
import Cup
import DetermineType
import DotNotation
import Folding
import Functor
import Hangman
import Hinq
import Lib
import Lists
import Monads
import Monoids as Mons
import MonoidSemigroup
import Polish
import Recursion
import Robot
import Route
import Testing
import TimeSeries
import Unfolds
import WordNumber


-- type FirstName = String
-- type LastName = String
-- type MiddleName = String
-- type Age = Int
-- type Height = Int
-- type PatientName = (FirstName, LastName)
-- data Name = Name FirstName LastName |
--             NameWM FirstName MiddleName LastName
-- data Sex = Male | Female
-- data RhType = Pos | Neg
-- data ABOType = A | B | AB | O
-- data BloodType = BloodType ABOType RhType

-- data Patient = Patient Name Sex Age Height BloodType
-- data PatientR = PatientR { name :: Name
--                          , sex :: Sex
--                          , age :: Age
--                          , height :: Height
--                          , bloodType :: BloodType}
-- class Describable a where
--   describe :: a -> String

-- data Icecream = Vanilla | Chocolate deriving (Show, Eq, Ord)

-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6
-- instance Show SixSidedDie where
--   show S1 = "one"
--   show S4 = "two"
--   show S3 = "three"
--   show _ = "other"
-- instance Eq SixSidedDie where
--   (==) S6 S6 = True
--   (==) S5 S5 = True
--   (==) S4 S4 = True
--   (==) S3 S3 = True
--   (==) S2 S2 = True
--   (==) S1 S1 = True
--   (==) _ _ = False

-- data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show,Enum,Bounded)

-- type Bits = [Bool]

-- firstName :: PatientName -> FirstName
-- firstName patientName = fst patientName

-- lastName :: PatientName -> FirstName
-- lastName patientName = snd patientName

-- showName :: Name -> String
-- showName (Name f l) = f ++ " " ++ l
-- showName (NameWM f m l) = f ++ " " ++ m ++ " " ++ l

-- patientInfo :: FirstName -> LastName -> Age -> Height -> String
-- patientInfo fname lname age height = name ++ " " ++ ageHeight
--   where name = lname ++ ", " ++ fname
--         ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

-- patientInfoV2 :: PatientName -> Age -> Height -> String
-- patientInfoV2 (fname,lname) age height = name ++ " " ++ ageHeight
--  where name = lname ++ ", " ++ fname
--        ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"


-- sexInitial :: Sex -> Char
-- sexInitial Male = 'M'
-- sexInitial Female = 'F'

-- showAbo :: ABOType -> String
-- showAbo A = "A"
-- showAbo B = "B"
-- showAbo AB = "AB"
-- showAbo O = "O"

-- showRh :: RhType -> String
-- showRh Pos = "Pos"
-- showRh Neg = "Neg"

-- showBloodType :: BloodType -> String
-- showBloodType (BloodType abo rht) = (showAbo abo) ++ " - "  ++(showRh rht)

-- canDonateTo :: BloodType -> BloodType -> Bool
-- canDonateTo (BloodType O _) _ = True
-- canDonateTo (BloodType A _) (BloodType A _)= True
-- canDonateTo (BloodType A _) (BloodType AB _)= True
-- canDonateTo (BloodType B _) (BloodType B _)= True
-- canDonateTo (BloodType B _) (BloodType AB _)= True
-- canDonateTo (BloodType AB _) (BloodType AB _)= True
-- canDonateTo (BloodType _ _) (BloodType _ _)= False

-- canDonateTo' :: PatientR -> PatientR -> Bool
-- canDonateTo' pa pb = canDonateTo (bloodType pa ) (bloodType pb)

-- rotN :: (Bounded a, Enum a) => Int -> a -> a
-- rotN alphabetSize c = toEnum rotation
--   where halfAlphabet = alphabetSize `div` 2
--         offset = fromEnum c + halfAlphabet
--         rotation = offset `mod` alphabetSize

-- rotChar :: Char -> Char
-- rotChar charToE = rotN sizeOfAlphabet charToE
--   where sizeOfAlphabet = largestCharNumber

-- largestCharNumber :: Int
-- largestCharNumber = 1 + fromEnum (maxBound::Char)

-- fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
-- fourLetterAlphabetEncoder vals = map rot4l vals
--   where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
--         rot4l = rotN alphaSize

-- xorBool :: Bool -> Bool -> Bool
-- xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

-- xorPair :: (Bool,Bool) -> Bool
-- xorPair (v1,v2) = xorBool v1 v2

-- xor :: [Bool] -> [Bool] -> [Bool]
-- xor a b = map xorPair (zip a b)

-- intToBits' :: Int -> Bits
-- intToBits' 0 = [False]
-- intToBits' 1 = [True]
-- intToBits' n = if remainder == 0
--               then False:(intToBits' nextValue)
--               else True:(intToBits' nextValue)
--   where remainder = n `mod` 2
--         nextValue = n `div` 2

-- intToBits :: Int -> Bits
-- intToBits n = leadingFalses ++ orderedBits
--   where orderedBits = reverse (intToBits' n)
--         missingBits = maxBits - (length orderedBits)
--         leadingFalses = take missingBits (cycle [False])

-- charToBits :: Char -> Bits
-- charToBits n = intToBits (fromEnum n)

-- maxBits :: Int
-- maxBits = length (intToBits' maxBound)

-- bitsToInt :: Bits -> Int
-- bitsToInt bits = sum (map ((2^) . snd) trueLocations)
--   where size = length bits
--         indices = [size-1, size-2 .. 0]
--         trueLocations = filter (((==) True) . fst) (zip bits indices)


-- type Events = [String]
-- type Probs = [Double]
-- data PTable = PTable Events Probs

-- createPTable :: Events -> Probs -> PTable
-- createPTable events probs = PTable events normalizedProbs
--  where
--   totalProbs      = sum probs
--   normalizedProbs = map (/ totalProbs) probs

-- instance Show PTable where
--   show (PTable events probs) = mconcat pairs
--     where pairs               = zipWith showPair events probs
--           showPair event prob = mconcat [event, ", ", show prob, "\n"]

-- cartesianCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
-- cartesianCombine f l1 l2 = zipWith f newL1 cycledL2
--   where
--     nToAdd     = length l2
--     repeatedL1 = map (replicate nToAdd) l1
--     newL1      = mconcat repeatedL1
--     cycledL2   = cycle l2

-- combineEvents :: Events -> Events -> Events
-- combineEvents = cartesianCombine (\x y -> mconcat [x, "-", y])

-- combineProbs :: Probs -> Probs -> Probs
-- combineProbs = cartesianCombine (*)

-- instance Semigroup PTable where
--   (<>) ptable1 (PTable [] []) = ptable1
--   (<>) (PTable [] []) ptable2 = ptable2
--   (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
--     where newEvents = combineEvents e1 e2
--           newProbs = combineProbs p1 p2

-- instance Monoid PTable where
--   mempty = PTable [] []
--   mappend = (<>)

-- toInts :: String -> [Int]
-- toInts = map read . lines

--helloPerson name = "Hello " ++ name ++ "!"

--sayHello = print "Whats your name?" >> getLine >>= return . helloPerson >>= putStrLn

f (a,b) (c, d) = ((b,d), (a, c))

main :: IO ()
main = do
  -- assessCandidateIO >>= print
  -- sayHello
  -- print "Whats your name?"
  -- name <- getLine
  -- let response = helloPerson name
  -- putStrLn response
  -- hSetBuffering stdin LineBuffering
  -- let aPatient = Patient (Name "Jordi" "Masramon") Male 50 91 (BloodType O Pos)
  -- let aPatientR = PatientR { name = Name "Jordi" "Masramon"
  --                          , sex = Male
  --                          , age = 50
  --                          , height = 91
  --                          , bloodType = BloodType O Pos}

  -- let anPatientR = aPatientR {age = 25}

  -- print (patientInfo "Masramon" "Jordi" 50 91)
  -- print ("Patient height = " ++ show (height aPatientR))
  -- print ("Patient age = " ++ show (age anPatientR))

  -- let message = [L1,L3,L4,L1,L1,L2]
  -- let encodedMes = fourLetterAlphabetEncoder message

  -- print ("[L1,L3,L4,L1,L1,L2] encoded to: " ++ show encodedMes)
  -- print ("[L3,L1,L2,L3,L3,L4] decoded to: " ++ show (fourLetterAlphabetEncoder encodedMes))

  {-
  assert ((curate (T.pack "False")) == (T.pack"false")) ":) curate working" ":( curate not working"
  assert ((curate (T.pack "FaLsE")) == (T.pack "false")) ":) curate working" ":( curate not working"
  assert (not ((curate (T.pack "FaLsE")) == (T.pack "FaLsE"))) ":) curate working" ":( curate not working"

  assert (not (isPalindrome (T.pack "False"))) ":) False not a palindrome" ":( False isPalindrome"
  assert (isPalindrome (T.pack "Racecar")) ":) Racecar isPalindrome" ":( Racecar not a palindrome"
  assert (isPalindrome (T.pack "racecar")) ":) racecar isPalindrome" ":( racecar not a palindrome"
  assert (isPalindrome (T.pack "racecar!")) ":) racecar! isPalindrome" ":( racecar! not a palindrome"
  -}
  -- userInput <- getContents
  -- let numbers = toInts userInput
  -- print (sum numbers)
  -- let reversed = reverse userInput
  -- putStrLn reversed
  -- mapM_ print userInput
  -- let n = length userInput
  -- putStr userInput
  -- print n

  print (roundTrip 4::Int)
  print (roundTrip '4'::Char)
  print (roundTrip "hola"::String)
  print (id 4)

  print "main done"
