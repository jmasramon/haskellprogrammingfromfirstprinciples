{-# LANGUAGE FlexibleInstances #-}

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

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant m) where 
  fmap _ (Constant v) = Constant v

data Bool = False | True --NO :k *

data BoolAndSomethingElse a =
  False' a | True' a deriving Show
instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

data BoolAndMaybeSomethingElse a =
  Falsish | Truish a deriving Show
instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

newtype Mu f  = InF  {outF::f  (Mu f)}

-- dataD=
--   D (Array Word Word) Int Int

data Sum' b a = First' a
  | Second' b
instance Functor (Sum' a) where 
  fmap f (First' a) = First' (f a) 
  fmap f (Second' b) = Second' b

data Company a c b = DeepBlue a c
  | Something b
instance Functor (Company e e') where 
  fmap f (Something b) = Something (f b) 
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a
  | R b a b
  deriving (Eq, Show)
instance Functor (More x) where 
  fmap f (L a b a')= L (f a) b (f a') 
  fmap f (R b a b')= R b (f a) b'

data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where 
  fmap f Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K a b= K a
instance Functor (K a) where 
  fmap f (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip (K' (f a))

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a = LiftItOut (f a)
instance (Functor f) => Functor (LiftItOut f) where
  fmap g (LiftItOut (fa)) = LiftItOut (fmap g fa)

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap k (DaWrappa (fa) (ga)) = DaWrappa (fmap k fa) (fmap k ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap k (IgnoringSomething (fa) (gb)) = IgnoringSomething (fa) (fmap k gb)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious (ga) (gb) (gt)) = Notorious (ga) (gb) (fmap f gt)

data List a = Nil
            | Cons a (List a)
instance Functor List where
  fmap f Nil = Nil 
  fmap f (Cons a lista) = Cons (f a) (fmap f lista)

data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
  fmap f NoGoat = NoGoat 
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats gl1 gl2 gl3) = MoreGoats (fmap f gl1) (fmap f gl2) (fmap f gl3)

data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)
instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read fsa) = Read (f . fsa)