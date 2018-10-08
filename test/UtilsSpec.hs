module UtilsSpec where

import           Test.Hspec
import           Utils

spec :: Spec
spec = do
    describe "iSqrt function" $ do
        it "[sqrt(5)] == 2" $ do
            iSqrt 5 `shouldBe` 2
        it "[sqrt(100)] == 10" $ do
            iSqrt 100 `shouldBe` 10
        it "[sqrt(17)] == 4" $ do
            iSqrt 17 `shouldBe` 4
    describe "isPrime function" $ do
        it "1 is not a prime" $ do
            isPrime 1 `shouldBe` False
        it "2 is a prime" $ do
            isPrime 2 `shouldBe` True
        it "3 is a prime" $ do
            isPrime 3 `shouldBe` True
        it "4 is not a prime" $ do
            isPrime 4 `shouldBe` False
        it "5 is a prime" $ do
            isPrime 5 `shouldBe` True
        it "17 is a prime" $ do
            isPrime 17 `shouldBe` True
        it "100 is not a prime" $ do
            isPrime 100 `shouldBe` False
    describe "slice function" $ do
        it "[0..5][1:3] == [1, 2]" $ do
            slice 1 3 [0..5] `shouldBe` ([1, 2] :: [Int])
        it "[0..][1:3] == [1, 2]" $ do
            slice 1 3 [0..] `shouldBe` ([1, 2] :: [Int])
        it "[0..5][3:3] == []" $ do
            slice 3 3 [0..5] `shouldBe` ([] :: [Int])
        it "[0..5][3:100] == [3, 4, 5]" $ do
            slice 3 100 [0..5] `shouldBe` ([3, 4, 5] :: [Int])
    describe "fixedLengthSublists" $ do
        it "fixedLengthSublists 0 [1..4] == [[]]" $ do
            fixedLengthSublists 0 [1..4] `shouldBe` ([[]] :: [[Int]])
        it "fixedLengthSublists 1 [1..4] == [[1], [2], [3], [4]]" $ do
            fixedLengthSublists 1 [1..4] `shouldBe` ([[1], [2], [3], [4]] :: [[Int]])
        it "fixedLengthSublists 2 [1..4] == [[1, 2], [2, 3], [3, 4]]" $ do
            fixedLengthSublists 2 [1..4] `shouldBe` ([[1, 2], [2, 3], [3, 4]] :: [[Int]])
        it "fixedLengthSublists 99 [1..4] == [[1, 2, 3, 4]]" $ do
            fixedLengthSublists 99 [1..4] `shouldBe` ([[1, 2, 3, 4]] :: [[Int]])
