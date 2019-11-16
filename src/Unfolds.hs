module Unfolds where

  
myIterate :: (a -> a) -> a -> [a] 
myIterate f x = x:(myIterate f (f x))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = go (f x) x 
  where
    go (Just (a, b)) x = a:(myUnfoldr f b)
    go Nothing _ = []


betterIterate :: (a -> a) -> a -> [a] 
betterIterate f = myUnfoldr (\x -> Just(x, f x))

