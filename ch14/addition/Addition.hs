module Addition where

import Test.Hspec
import Test.QuickCheck

recMult :: (Eq a, Num a) => a -> a -> a
recMult _ 0 = 0
recMult 0 _ = 0
recMult n 1 = n
recMult 1 m = m
recMult n m = n + recMult n (m - 1)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

main :: IO ()
main = hspec $ describe "Recursive Multiplication" $ do
  it "1 * 1 is 1" $
    recMult 1 1 `shouldBe` 1
  it "0 * 2 is 0" $
    recMult 0 2 `shouldBe` 0 
  it "3 * 5 is 15" $
    recMult 3 5 `shouldBe` 15 
  it "5 * 3 is 15" $
    recMult 5 3 `shouldBe` 15
  it "x + 1 is always greater than x" $
    property $ \x -> x + 1 > (x :: Int)