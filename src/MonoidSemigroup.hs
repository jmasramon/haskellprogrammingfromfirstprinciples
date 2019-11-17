module MonoidSemigroup where

import Data.Monoid

data Optional a = Nada
                | Only a
                  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) (Nada) (Nada) = Nada
  (<>) (Nada) (Only y) = Only y
  (<>) (Only x) (Nada)= Only x
  (<>) (Only x) (Only y)= Only (x <> y)


instance Monoid a => Monoid (Optional a) where 
  mempty = Nada
  mappend = (<>)

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
            -> Adverb
            -> Noun
            -> Adjective
            -> String
madlibbin' e adv noun adj =
  e <>"! he said "<> adv<>" as he jumped into his car "<> noun <> " and drove off with his " <> adj <> " wife."

madlibbinBetter' :: Exclamation
  -> Adverb
  -> Noun
  -> Adjective
  -> String
madlibbinBetter' e adv noun adj = 
  mconcat [e, "! he said ", 
    adv, " as he jumped into his car ", 
    noun, " and drove off with his " , 
    adj , " wife."]


newtype First' a =
  First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' Nada) (First' Nada) = First' Nada
  (<>) (First' Nada) (First' z@(Only _)) = First' z
  (<>) (First'  z@(Only _)) (First' Nada)= First' z
  (<>) (First' z@(Only _)) (First' (Only _))= First' z

instance Monoid (First' a) where 
  mempty = (First' Nada)
  mappend = (<>)

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend


type FirstMappend =
    First' String
  -> First' String
  -> First' String
  -> Bool
type FstId = First' String -> Bool

data Trivial = Trivial deriving (Eq, Show) 

instance Semigroup Trivial where
  _ <> _ = Trivial

newtype Identity a = Identity a

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x<>y)

data Two a b = Two a b

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two z k )= Two (x<>z) (y<>k)

data Three a b c = Three a b c

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x y z) <> (Three k l m )= Three (x<>k) (y<>l) (z<>m)

--same for Four

newtype BoolConj = BoolConj Bool deriving Show

instance Semigroup BoolConj where
  BoolConj x <> BoolConj y = BoolConj (x && y)

newtype BoolDisj = BoolDisj Bool deriving Show

instance Semigroup BoolDisj where
  BoolDisj x <> BoolDisj y = BoolDisj (x || y)

data Or a b = 
    Fst a
  | Snd b deriving Show

instance Semigroup (Or a b) where
  (<>) (Fst _) (Snd y) = Snd y
  (<>) (Snd x) (_) = Snd x
  (<>) (Fst _) (Fst y) = Fst y
  
newtype Combine a b = Combine { unCombine :: (a -> b) } 

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine (f <> g)
