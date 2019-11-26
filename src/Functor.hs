module Functor where

a = fmap (+1) $ read "[1]" :: [Int]

b =(fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = fmap (*2) (\x->x-2)

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let 
      ioi = readIO "1" :: IO Integer
      changed = fmap read $ fmap (("123"++) . show) ioi 
    in fmap (*3) changed

newtype Identity a = Identity a
instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

data Pair a = Pair a a
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

data Two a b = Two a b
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c
instance Functor (Three a b) where
  fmap f (Three a b c ) = Three a b (f c)

data  Three' a b = Three' a b b 
instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

data  Four a b c d = Four a b c d
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data  Four' a b = Four' a a a b
instance Functor (Four' a) where
  fmap f (Four' x y z k) = Four' x y z (f k)

data Trivial = Trivial --Cannot be implemeted fo this one as :k = *
data Possibly a = LolNope
  | Yeppers a 
  deriving (Eq, Show)

instance Functor Possibly where 
  fmap f (Yeppers x)= Yeppers (f x)
  fmap _ (LolNope)= LolNope

data Sum a b =  First a
              | Second b deriving (Eq, Show)
instance Functor (Sum a) where 
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)