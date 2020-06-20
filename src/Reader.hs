{-# LANGUAGE InstanceSigs #-}
module Reader where

import Control.Applicative
import Data.Char

boop :: Num a => a -> a
boop = (*2)

doop :: Num a => a -> a
doop = (+10)

-- using functor wiht functions ((->) a)
bip :: Num a => a -> a
bip = fmap boop doop -- == boop . doop
-- fmap f fx = f(f x)
-- fmap ((->) a) ((->) a) = ((->) a ((->) a)) 

-- three ways of using applicative wiht functions ((->) a)
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
boopDoop = do
    a <- boop
    b <- doop
    return (a + b)

-- First Exercises
cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap

mTupled :: [Char] -> ([Char], [Char])
mTupled = do
    c <- cap
    r <- rev
    return (r,c)

-- did not do this one myself !!!
mTupled' :: [Char] -> ([Char], [Char])
mTupled' = rev >>= \x -> cap >>= \y -> return (x,y)

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person =
    Person {
        humanName :: HumanName
        , dogName :: DogName
        , address :: Address
    } deriving (Eq, Show)
data Dog =
    Dog {
        dogsName :: DogName
        , dogsAddress :: Address
    } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")
chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- Exercise: Reading Comprehension
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ f . ra
    
instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ const a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

