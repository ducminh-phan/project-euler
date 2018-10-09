module PE1Spec where

import           PE1
import           Test.Hspec

spec :: Spec
spec = do
    describe "1" $ do
        it "The sum of all the multiples of 3 or 5 below 10 equals 23" $
            sumOfMultiplesOf3Or5Below 10 `shouldBe` 23
        it "The sum of all the multiples of 3 or 5 below 1000 equals 233168" $
            sumOfMultiplesOf3Or5Below 1000 `shouldBe` 233168
    describe "2" $ do
        it "The first 5 even Fibonacci numbers are 2, 8, 34, 144, 610" $
            take 5 evenFibs `shouldBe` [2, 8, 34, 144, 610]
        it "The sum of all even Fibonacci numbers below 1000 is 798" $
            sumOfEvenFibsBelow 1000 `shouldBe` 798
    describe "3" $ do
        it "the reduction of a number by a prime factor is correctly computed" $ do
            reduceDiv 32 2 `shouldBe` 1
            reduceDiv 5040 2 `shouldBe` 315
            reduceDiv 315 3 `shouldBe` 35
        it "all prime factors are correctly computed" $ do
            primeFactors 13195 `shouldBe` [5, 7, 13, 29]
            primeFactors 5040 `shouldBe` [2, 3, 5, 7]
            primeFactors 123456 `shouldBe` [2, 3, 643]
        it "the largest prime factor is correctly computed" $ do
            largestPrimeFactor 13195 `shouldBe` 29
            largestPrimeFactor 5040 `shouldBe` 7
            largestPrimeFactor 123456 `shouldBe` 643
    describe "4" $ do
        it "a number is correctly determined to be palindromic or not" $ do
            isPalindromic 123 `shouldBe` False
            isPalindromic 98789 `shouldBe` True
        it "The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 * 99" $
            largestPalindrome 2 `shouldBe` 9009
    describe "5" $
        it "2520 is the smallest number that is divisible by each number from 1 to 10" $
            smallestMultiple 10 `shouldBe` 2520
    describe "6" $
        it ("The difference between the sum of the squares and the square of the sum"
         ++ " of the first 10 natural numbers is 2640") $
            sumSquareDifference 10 `shouldBe` 2640
    describe "7" $
        it "the n-th prime is correctly computed" $ do
            nthPrimeNumber 1 `shouldBe` 2
            nthPrimeNumber 5 `shouldBe` 11
            nthPrimeNumber 10 `shouldBe` 29
            nthPrimeNumber 100 `shouldBe` 541
    describe "8" $
        it "The maximum product of 4 consecutive digits is 5832" $
            maxProduct 4 `shouldBe` 5832
    describe "10" $
        it "The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17" $
            sumOfPrimesBelow 10 `shouldBe` 17
