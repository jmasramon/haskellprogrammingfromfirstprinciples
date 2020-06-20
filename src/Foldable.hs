module Foldable where

import Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . Foldable.foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . Foldable.foldMap Product

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
fold = Foldable.foldMap id

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f = foldr (\y acc -> f y <> acc) mempty


