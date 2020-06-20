module FunctorSpec where

import Functor

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

    
functorIdentity :: (Functor f, Eq (f a)) => f a -> Prelude.Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Prelude.Bool
functorCompose (Fun _ f) (Fun _ g) x = fmap g (fmap f x) == fmap (g . f) x

instance (Arbitrary a) => Arbitrary (Identity a) where 
    arbitrary = do
        s <- arbitrary
        return (Identity s)

type IdentityFIdentity = Identity String -> Prelude.Bool
type IntToInt = Fun Int Int
type IntFC = IntToInt -> IntToInt -> Identity Int -> Prelude.Bool

instance (Arbitrary a) => Arbitrary (Pair a) where 
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Pair x y)

type IdentityPair = Pair String -> Prelude.Bool
type IntPair = IntToInt -> IntToInt -> Pair Int -> Prelude.Bool


spec :: Spec
spec = do
    describe "Test Functor implementations" $ do
        it "Identity identity" $ do
            property (functorIdentity :: IdentityFIdentity)
        it "Identity compose" $ do
            property (functorCompose :: IntFC)
        it "Pair identity" $ do
            property (functorIdentity :: IdentityPair)
        it "Pair compose" $ do
            property (functorCompose :: IntPair)
        