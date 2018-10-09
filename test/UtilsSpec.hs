module UtilsSpec where

import           Test.Hspec
import           Utils

spec :: Spec
spec = do
    describe "iSqrt function" $ do
        it "[sqrt(5)] == 2" $
            iSqrt 5 `shouldBe` 2
        it "[sqrt(100)] == 10" $
            iSqrt 100 `shouldBe` 10
        it "[sqrt(17)] == 4" $
            iSqrt 17 `shouldBe` 4
    describe "isPrime function" $ do
        it "1 is not a prime" $
            isPrime 1 `shouldBe` False
        it "2 is a prime" $
            isPrime 2 `shouldBe` True
        it "3 is a prime" $
            isPrime 3 `shouldBe` True
        it "4 is not a prime" $
            isPrime 4 `shouldBe` False
        it "5 is a prime" $
            isPrime 5 `shouldBe` True
        it "17 is a prime" $
            isPrime 17 `shouldBe` True
        it "100 is not a prime" $
            isPrime 100 `shouldBe` False
    describe "slice function" $ do
        it "[0..5][1:3] == [1, 2]" $
            slice 1 3 [0..5] `shouldBe` [1, 2]
        it "[0..][1:3] == [1, 2]" $
            slice 1 3 [0..] `shouldBe` [1, 2]
        it "[0..5][3:3] == []" $
            slice 3 3 [0..5] `shouldBe` []
        it "[0..5][3:100] == [3, 4, 5]" $
            slice 3 100 [0..5] `shouldBe` [3, 4, 5]
    describe "fixedLengthSublists" $ do
        it "fixedLengthSublists 0 [1..4] == [[]]" $
            fixedLengthSublists 0 [1..4] `shouldBe` [[]]
        it "fixedLengthSublists 1 [1..4] == [[1], [2], [3], [4]]" $
            fixedLengthSublists 1 [1..4] `shouldBe` [[1], [2], [3], [4]]
        it "fixedLengthSublists 2 [1..4] == [[1, 2], [2, 3], [3, 4]]" $
            fixedLengthSublists 2 [1..4] `shouldBe` [[1, 2], [2, 3], [3, 4]]
        it "fixedLengthSublists 99 [1..4] == [[1, 2, 3, 4]]" $
            fixedLengthSublists 99 [1..4] `shouldBe` [[1, 2, 3, 4]]
