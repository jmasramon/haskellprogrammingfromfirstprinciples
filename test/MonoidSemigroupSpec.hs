module MonoidSemigroupSpec where

import MonoidSemigroup

import Test.Hspec
import Test.QuickCheck

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
type TrivialLeftId = Trivial -> Bool

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do 
        a <- arbitrary
        return (Identity a)
    
type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool
type IdentityLeftId = Identity String -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do 
        a <- arbitrary
        b <- arbitrary
        return (Two a b)
    
type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool
type TwoLeftId = Two String String -> Bool

instance Arbitrary BoolConj where
    arbitrary = BoolConj <$> arbitrary
    
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolConjLeftId = BoolConj -> Bool


spec :: Spec
spec = do
    describe "Test Semigroup implementations" $ do
        it "trivial Assoc" $ do
            property (semigroupAssoc :: TrivialAssoc)
        it "Identity Assoc" $ do
            property (semigroupAssoc :: IdentityAssoc)
        it "Two Assoc" $ do
            property (semigroupAssoc :: TwoAssoc)
        it "Two Assoc" $ do
            property (semigroupAssoc :: BoolConjAssoc)
    describe "Test Monoid implementations" $ do
        it "trivial Assoc" $ do
            property (monoidLeftIdentity :: TrivialLeftId)
        it "Identity Assoc" $ do
            property (monoidLeftIdentity :: IdentityLeftId)
        it "Two Assoc" $ do
            property (monoidLeftIdentity :: TwoLeftId)
        it "Two Assoc" $ do
            property (monoidLeftIdentity :: BoolConjLeftId)
        it "trivial Assoc" $ do
            property (monoidRightIdentity :: TrivialLeftId)
        it "Identity Assoc" $ do
            property (monoidRightIdentity :: IdentityLeftId)
        it "Two Assoc" $ do
            property (monoidRightIdentity :: TwoLeftId)
        it "Two Assoc" $ do
            property (monoidRightIdentity :: BoolConjLeftId)
                        