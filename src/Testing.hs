module Testing where

import Test.Hspec
import Test.QuickCheck
import Recursion

sayHello :: String
sayHello = "hello!"

testAddition :: IO () 
testAddition = hspec $ do
  describe "Addition" $ do 
    it "1+1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it"2+2 is equal to 4"$do
        2 + 2 `shouldBe` 4
  describe "say Hello" $ do
    it "sayHello should say hello!" $ do
      sayHello `shouldBe` "hello!"
  describe "recMult" $ do
    it "5*3 should be 15" $ do
      recMult 5 3 `shouldBe` 15
    it "3*5 should also be 15" $ do
      recMult 3 5 `shouldBe` 15
    it "mutitplying by 1 should not change input" $ do
      property $ \x -> recMult x 1 == (x::Int) 
    it "mutitplying to 1 should not change input" $ do
      property $ \x -> recMult 1 x == (x::Int) 
    it "mutitplying by 0 should return 0" $ do
      property $ \x -> recMult 0 x == (0::Int) 
    it "mutitplying to 0 should return 0" $ do
      property $ \x -> recMult x 0 == (0::Int) 
  describe "property testing" $ do
    it"x+1 is always greater than x" $ do 
      property $ \x -> x+1 > (x::Int)
  describe "addition properties" $ do 
    it "associativity" $ do
      property prop_plusAssociative 
    it "commutatifitty" $ do
      property prop_plusCommutative 
    
-- Practicing generators
trivialInt :: Gen Int
trivialInt = return 2

oneThroughThree :: Gen Int 
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b) 
genTuple = do
  a <- arbitrary 
  b <- arbitrary 
  return (a, b)

-- Some more tests
plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y+z) == (x+y)+z

prop_plusAssociative :: Int -> Int -> Int -> Bool
prop_plusAssociative x y z = plusAssociative x y z

plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y = x+y==y+x

prop_plusCommutative :: Int -> Int -> Bool
prop_plusCommutative x y = x+y==y+x

-- Gen random generators
data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

foolGen:: Gen (Fool)
foolGen = oneof [return Fulse, return Frue]