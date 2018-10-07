module PE1Spec where

import           PE1
import           Test.Hspec

spec :: Spec
spec = do
    describe "1" $ do
        it "The sum of all the multiples of 3 or 5 below 10 equals 23" $ do
            sumOfMultiplesOf3Or5Below 10 `shouldBe` (23 :: Int)
        it "The sum of all the multiples of 3 or 5 below 1000 equals 233168" $ do
            sumOfMultiplesOf3Or5Below 1000 `shouldBe` (233168 :: Int)
