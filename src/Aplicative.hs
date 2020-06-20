module Aplicative where

import Data.List (elemIndex)
import Control.Applicative (liftA3)

-- Exercises: Lookup
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4,5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z  -- (,) <$> y <*> z -- liftA2 (,) y z

x :: Maybe Int
x = elemIndex 3 [1,2,3,4,5]

k :: Maybe Int
k = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed= max' <$> x <*> k

xs=[1,2,3]
ys=[4,5,6]

l :: Maybe Integer
l=lookup 3 $ zip xs ys

m :: Maybe Integer
m=lookup 2 $ zip xs ys

summed :: Maybe Integer
summed= fmap sum $ pure (,) <*> l <*> m

-- Exercise: Identity instance
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)
  -- (<*>) (Identity f) ix = fmap f ix

-- Exercise: Constant instance
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ Constant{getConstant = x} = Constant {getConstant = x}

instance Monoid a => Applicative (Constant a) where
  pure x = Constant {getConstant = mempty}
  (<*>) Constant {getConstant = x} Constant{getConstant = y} = Constant{getConstant = mappend x y }

-- Exercise: Fixer Upper
res = const <$> Just "Hello" <*> pure "World"

res2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- Exercise for List

data List a = Nil
            | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) Nil ys = ys
  (<>) (Cons x xs) ys = Cons x $ xs <> ys

instance Monoid (List a) where
  mempty = Nil

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  --(<*>) (Cons f fs) as = fmap f as <> (<*>) fs as
  (<*>) fs xs = flatMap (`fmap` xs) fs

append::List a->List a->List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold::(a->b->b)->b->List a->b
fold _ acc Nil = acc
fold f acc (Cons h t) = f h (fold f acc t)

concat' :: List (List a) -> List a
concat' = fold append Nil

  -- write this one in terms of concat' and fmap
flatMap::(a->List b)->List a->List b
flatMap f as = concat' $ fmap f as

-- toMyList = foldr Cons Nil

-- Exercise for ZipList
data List' a =
  Nil'
  | Cons' a (List' a)
  deriving (Eq, Show)
  
-- instance Functor List' where
--   fmap = undefined

-- instance Applicative List' where
--   pure = undefined
--   (<*>) = undefined

take' :: Int -> List a -> List a
take' 0 xs = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' a) = ZipList' (fmap f a)

instance Applicative ZipList' where
  pure = ZipList' . repeat'
    where repeat' a = Cons a (repeat' a)
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' (zipWith' fs xs)

zipWith':: List (a->b) -> List a -> List b
zipWith' Nil xs = Nil
zipWith' fs Nil = Nil
zipWith' (Cons f fs) (Cons x xs) = Cons (f x) (zipWith' fs xs)

-- Validation exercise
data Validation e a = Failure e
                    | Success a deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

  -- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Success f ) (Success y) = Success (f y)
  (<*>) (Success x ) (Failure y) = Failure y
  (<*>) (Failure y) (Success x ) = Failure y
  (<*>) (Failure x ) (Failure y) = Failure (x `mappend` y)

--Chapter exercises

-- pure :: a -> [a]
-- (<*>) :: [a -> b] -> [a] -> [b]

-- pure ::a-> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- (,) e
-- pure :: a -> (e, a)
-- (<*>) :: (e, a -> b) -> (e, a) -> (e, b)

-- (->) e
-- pure :: a -> (e->a)
-- (<*>) :: (e -> (a -> b)) -> (e->a) -> (e->a)

data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f f') (Pair x x') = Pair (f x) (f' x')

data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two x f') (Two y y') = Two (x <> y) (f' y')

data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three x y f') (Three z z' z'') = Three (x <> z) (y <> z') (f' z'')

data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' x y y') = Three' x (f y) (f y')

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' x f f') (Three' z z' z'') = Three' (x <> z) (f z') (f' z'')

data Four a b c d = Four a b c d
data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' x y z k) = Four' x y z (f k)

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' x y z f) (Four' x' y' z' k) = Four' (x <> x') (y <> y') (z <> z') (f k)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combineLetters:: a -> b -> c -> (a, b, c)
combineLetters x y z = (x, y, z)

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
-- combos = liftA3 combineLetters
combos = liftA3 (,,)
>>>>>>> 835782e95e1ee8310dd7108ef2220aedee5fa90e
