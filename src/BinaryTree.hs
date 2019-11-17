module BinaryTree where

data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f levels = go (f levels) 
  where
    go (Just (x, y, z)) = Node (unfold f x) y (unfold f z)
    go Nothing = Leaf

treeBuild :: Integer -> BinaryTree Integer 
treeBuild n = unfold f 0
    where f acc = if (acc == n) then Nothing else Just (acc+1, acc, acc+1)

treeBuild' :: Integer -> BinaryTree Integer 
treeBuild' n = go 0
  where 
    go curLevVal = 
      if (n == curLevVal) 
        then Leaf
        else Node (go (curLevVal +1)) curLevVal (go (curLevVal +1))