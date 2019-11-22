module Monoids where

import Control.Monad
import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> (Only a) = Only a
  (Only a) <> Nada = Only a
  (Only a) <> (Only a') = Only (a <> a')
  Nada <> Nada = Nada

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
