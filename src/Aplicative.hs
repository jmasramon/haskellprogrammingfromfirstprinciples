module Aplicative where

import Data.List (elemIndex)

-- Exercises: Lookup
added :: Maybe Integer
added = pure (+3) <*> (lookup 3 $ zip [1, 2, 3] [4,5, 6])

y :: Maybe Integer 
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer 
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer) 
tupled = pure (,) <*> y <*> z  -- (,) <$> y <*> z -- liftA2 (,) y z
  
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
  fmap _ (Constant {getConstant = x}) = Constant {getConstant = x}

instance Monoid a => Applicative (Constant a) where 
  pure x = Constant {getConstant = mempty}
  (<*>) Constant {getConstant = x} (Constant {getConstant = y}) = Constant{getConstant = mappend x y }

-- Exercise: Fixer Upper
res = const <$> Just "Hello" <*> pure "World"

res2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]


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

-- instance Applicative List where 
--   pure a = Cons a Nil
--   (<*>) Nil _ = Nil
--   (<*>) _ Nil = Nil
--   (<*>) (Cons f Nil) (Cons a Nil) = Cons (f a) Nil
--   (<*>) (Cons f Nil) (Cons a la) = Cons (f a) ((<*>) (Cons f Nil) la)
--   --(<*>) (Cons f (Cons g lg)) (Cons a Nil) = Cons (f a) ((<*>) (Cons g lg) (Cons a Nil))   
--   (<*>) (Cons f (Cons g lg)) (Cons a la) = ((<*>) (Cons f Nil) (Cons a la)) <> ((<*>) (Cons g lg) (Cons a la))

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

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) xs ys = undefined  

