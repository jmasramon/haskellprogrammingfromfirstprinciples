module Monad where

import Control.Monad (join, liftM3, liftM2, liftM)
import Control.Applicative (liftA3)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f


twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
  then [x*x, x*x]
  else [x*x]

doubleIfEven::Integer -> [Integer]
doubleIfEven x =
  if even x
    then [x*x, x*x]
    else [x*x]

twiceOnlyWhenEven :: [Integer] -> [Integer]
twiceOnlyWhenEven xs = do
    x <- xs
    if even x
    then [x*x, x*x]
    else []

data Cow = Cow {
      name :: String
      , age :: Int
      , weight :: Int
      } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
    | n >= 0 = Just n
    | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in
    if n == "Bess" && w > 499
      then Nothing
      else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' = do
  name <- noEmpty name'
  age <- noNegative age'
  let cow = Cow name age weight'
  weightCheck cow

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' =
  noEmpty name' >>= \name -> noNegative age' >>= \age -> weightCheck (Cow name age weight')

liftedCow = liftM3 Cow

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  liftedCow (noEmpty name') (noNegative age') (Just weight') >>= weightCheck

doSomething f g h = do
  a <- f
  b <- g
  c <- h
  pure (a, b, c)

doSomething' f g h = liftA3 (,,) f g h

doSomething'' f g h =
  \n -> do
    a <- f n
    b <- g a
    c <- h b
    pure (a, b, c)


doSomething''' f g h n =
  liftA3 (,,) (f n) (g (f n)) (h (g (f n)))

-- Either exercise

data Sum a b = First a
              | Second b deriving (Eq, Show)
instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure  = Second
  (<*>) (First a) _ = First a
  (<*>) (Second _) (First a) = First a
  (<*>) (Second f) (Second a) = Second (f a)

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _= First a
  (>>=) (Second b) f = f b

-- chapter exercises

data Nope a = NopeDotJpg

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  (>>=) _ _ = NopeDotJpg

data PhhhbbtttEither b a = Lefty a
                          | Righty b

instance Functor (PhhhbbtttEither b) where
  fmap f (Lefty a) = Lefty (f a)
  fmap f (Righty b) = Righty b

instance Applicative (PhhhbbtttEither b) where
  pure = Lefty
  (<*>) (Lefty f) (Lefty a) = Lefty (f a)
  (<*>) (Righty b)  _ = Righty b
  (<*>) (Lefty _) (Righty  b) = Righty b

instance Monad  (PhhhbbtttEither b) where
  return = Lefty
  (>>=) (Lefty a) f = f a
  (>>=) (Righty b) _ = Righty b

-----

j :: Monad m => m (m a) -> m a
--j = join
j = (>>= id)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
--l2 = liftM2
l2 f ma mb = f <$> ma <*> mb

a' :: Monad m => m a -> m (a -> b) -> m b
a' = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a:as) famb = liftM2 (:) (famb a) (meh as famb)

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id