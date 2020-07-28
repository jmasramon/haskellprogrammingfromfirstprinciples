module Fldable where

import Data.Monoid
import Data.Foldable

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr (\y acc -> acc || y == x) False 


minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr myMin Nothing
    where 
        myMin x Nothing = Just x
        myMin x (Just y) = Just $ min x y


maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr myMax Nothing
    where 
        myMax x Nothing = Just x
        myMax x (Just y) = Just $ max x y


null :: (Foldable t) => t a -> Bool
--null = (== 0) . Foldable.length
null = foldr (\y acc -> False) True


length :: (Foldable t) => t a -> Int
length = foldr (\_ acc -> acc + 1) 0


toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap f = foldr (\y acc -> f y <> acc) mempty

-------------------------------------------------
-- Exercises
data Constant a b = Constant a deriving (Show, Eq)

instance Foldable (Constant a) where
    foldMap _ (Constant x) = mempty

data Two a b = Two a b deriving (Show, Eq)

instance Foldable (Two a) where
    foldMap f (Two x y) = f y
    
data Three a b c = Three a b c deriving (Show, Eq)

instance Foldable (Three a b) where
    foldMap f (Three x y z) = f z
    
data Three' a b = Three' a b b deriving (Show, Eq)

instance Foldable (Three' a) where
    foldMap f (Three' x y z) = f y <> f z
    
data Four' a b = Four' a b b b deriving (Show, Eq)
instance Foldable (Four' a) where
    foldMap f (Four' x y z l) = f y <> f z <> f l

