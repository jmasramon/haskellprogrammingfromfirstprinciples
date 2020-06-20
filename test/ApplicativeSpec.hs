module ApplicativeSpec where

import Aplicative

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

instance (Eq a) =>EqProp (Identity a) where (=-=) = eq

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary
        xs <- arbitrary
        elements [Nil, Cons x xs]
instance (Eq a) =>EqProp (List a) where (=-=) = eq

instance (Arbitrary a) => Arbitrary (ZipList' a) where
    arbitrary = do
        x <- arbitrary
        return (ZipList' x)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
      where xs' = let (ZipList' l) = xs
              in take' 500 l
            ys' = let (ZipList' l) = ys
              in take' 500 l
instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [Aplicative.Failure x, Aplicative.Success y]
instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "Test Semigroup implementations" $ do
        it "trivial Assoc" $ do
            (3 == 3) `shouldBe` True
        it "[] applicative" $ do
            quickBatch $ applicative [("b", "w", 1::Int)]
        it "Identity applicative" $ do
            quickBatch $ applicative (undefined :: Identity (Integer, Integer, Integer))
        it "List applicative" $ do
            quickBatch $ applicative (undefined :: List (Integer, Integer, Integer))
        it "ZipList' applicative" $ do
            quickBatch $ applicative (undefined :: ZipList' (Integer, Integer, Integer))
        it "Validation applicative" $ do
            quickBatch $ applicative (undefined :: Validation String (Integer, Integer, Integer))