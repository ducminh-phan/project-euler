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
    describe "2" $ do
        it "The first 5 even Fibonacci numbers are 2, 8, 34, 144, 610" $ do
            take 5 evenFibs `shouldBe` [2, 8, 34, 144, 610]
        it "The sum of all even Fibonacci numbers below 1000 is 798" $ do
            sumOfEvenFibsBelow 1000 `shouldBe` 798
    describe "3" $ do
        it "32 reduced by 2 is 1" $ do
            reduceDiv 32 2 `shouldBe` 1
        it "5040 reduced by 2 is 315" $ do
            reduceDiv 5040 2 `shouldBe` 315
        it "315 reduced by 3 is 35" $ do
            reduceDiv 315 3 `shouldBe` 35
        it "The prime factors of 13195 are 5, 7, 13, and 29" $ do
            primeFactors 13195 `shouldBe` [5, 7, 13, 29]
        it "The prime factors of 5040 are 2, 3, 5, and 7" $ do
            primeFactors 5040 `shouldBe` [2, 3, 5, 7]
        it "The prime factors of 123456 are 2, 3, and 643" $ do
            primeFactors 123456 `shouldBe` [2, 3, 643]
        it "The largest prime factor of 13195 is 29" $ do
            largestPrimeFactor 13195 `shouldBe` 29
        it "The largest prime factor of 5040 is 7" $ do
            largestPrimeFactor 5040 `shouldBe` 7
        it "The largest prime factor of 123456 is 643" $ do
            largestPrimeFactor 123456 `shouldBe` 643
    describe "4" $ do
        it "123 is not a palindromic number" $ do
            isPalindromic 123 `shouldBe` False
        it "98789 is a palindromic number" $ do
            isPalindromic 98789 `shouldBe` True
        it "The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 * 99" $ do
            largestPalindrome 2 `shouldBe` 9009
    describe "5" $ do
        it "2520 is the smallest number that is divisible by each number from 1 to 10" $ do
            smallestMultiple 10 `shouldBe` 2520
    describe "6" $ do
        it ("The difference between the sum of the squares and the square of the sum"
         ++ " of the first 10 natural numbers is 2640") $ do
            sumSquareDifference 10 `shouldBe` 2640
    describe "7" $ do
        it "The first prime number is 2" $ do
            nthPrimeNumber 1 `shouldBe` 2
        it "The fifth prime number is 11" $ do
            nthPrimeNumber 5 `shouldBe` 11
        it "The 10th prime number is 29" $ do
            nthPrimeNumber 10 `shouldBe` 29
        it "The 100th prime number is 541" $ do
            nthPrimeNumber 100 `shouldBe` 541
    describe "8" $ do
        it "The maximum product of 4 consecutive digits is 5832" $ do
            maxProduct 4 `shouldBe` 5832
    describe "10" $ do
        it "The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17" $ do
            sumOfPrimesBelow 10 `shouldBe` 17
