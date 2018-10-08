module PE1 where

import           Data.Char           (digitToInt)
import           Data.List           (sort)
import           Data.Numbers.Primes (primes)
import           Utils

-- Problem #1

sumOfMultiplesOf3Or5Below :: Int -> Int
sumOfMultiplesOf3Or5Below x = sum . filter isMultipleOf3Or5 $ [1 .. x - 1]
  where
    isMultipleOf3Or5 t = t `mod` 3 == 0 || t `mod` 5 == 0

-- Problem #2

-- | Compute even Fibonacci numbers
-- The recursive formula is FE(n) = 4 * FE(n - 1) + FE(n - 2)
evenFibs :: [Int]
evenFibs = 2 : 8 : zipWith (+) evenFibs (map (* 4) $ tail evenFibs)

-- | Compute the sum of all even Fibonacci numbers below the given threshold
sumOfEvenFibsBelow :: Int -> Int
sumOfEvenFibsBelow x = sum . takeWhile (< x) $ evenFibs

-- Problem #3

-- | Reduce a number n by a divisor d until n is not divisible by d
reduceDiv :: Int -> Int -> Int
reduceDiv n d
    | n `mod` d == 0 = reduceDiv (n `div` d) d
    | otherwise = n

-- | Compute all prime factors of a given number
primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n 2
  where
    primeFactors' n' d
        | n' == 1 = []
        | n' `mod` d == 0 = d : primeFactors' (reduceDiv n' d) d
        | otherwise = primeFactors' n' (d + 1)

-- | Compute the largest prime factor of a given number
largestPrimeFactor :: Int -> Int
largestPrimeFactor = last . primeFactors

-- Problem #4

-- | Construct the list of n-digit numbers
nDigitNumbers :: Int -> [Int]
nDigitNumbers n = [10 ^ (n - 1) .. 10 ^ n - 1]

-- | Construct the list of products of two n-digit numbers, sorted in the decreasing order
nDigitNumbersProduct :: Int -> [Int]
nDigitNumbersProduct n = reverse . sort $ [x * y | x <- nDigitNumbers n, y <- nDigitNumbers n]

-- | Check if a number is palindromic
isPalindromic :: Int -> Bool
isPalindromic n = (show n) == (reverse . show $ n)

-- | Find the largest palindrome made from the product of two n-digit numbers
largestPalindrome :: Int -> Int
largestPalindrome = head . filter isPalindromic . nDigitNumbersProduct

-- Problem #5

-- | Find the smallest positive number that is evenly divisible by all of the numbers from 1 to n
smallestMultiple :: Int -> Int
smallestMultiple n = foldr lcm 1 [1 .. n]

-- Problem #6

-- | Find the difference between the sum of the squares and the square of the sum of the first n numbers
-- We use the explicit formulae for each term and subtract them
sumSquareDifference :: Int -> Int
sumSquareDifference m = squareOfSum m - sumOfSquare m
  where
    squareOfSum n = n * n * (n + 1) * (n + 1) `div` 4
    sumOfSquare n = n * (n + 1) * (2 * n + 1) `div` 6

-- Problem #7

-- | Find the n-th prime number
nthPrimeNumber :: Int -> Int
nthPrimeNumber n = last . take n $ primes

-- Problem #8

numStr :: [Char]
numStr = "73167176531330624919225119674426574742355349194934"
      ++ "96983520312774506326239578318016984801869478851843"
      ++ "85861560789112949495459501737958331952853208805511"
      ++ "12540698747158523863050715693290963295227443043557"
      ++ "66896648950445244523161731856403098711121722383113"
      ++ "62229893423380308135336276614282806444486645238749"
      ++ "30358907296290491560440772390713810515859307960866"
      ++ "70172427121883998797908792274921901699720888093776"
      ++ "65727333001053367881220235421809751254540594752243"
      ++ "52584907711670556013604839586446706324415722155397"
      ++ "53697817977846174064955149290862569321978468622482"
      ++ "83972241375657056057490261407972968652414535100474"
      ++ "82166370484403199890008895243450658541227588666881"
      ++ "16427171479924442928230863465674813919123162824586"
      ++ "17866458359124566529476545682848912883142607690042"
      ++ "24219022671055626321111109370544217506941658960408"
      ++ "07198403850962455444362981230987879927244284909188"
      ++ "84580156166097919133875499200524063689912560717606"
      ++ "05886116467109405077541002256983155200055935729725"
      ++ "71636269561882670428252483600823257530420752963450"

digitList :: [Int]
digitList = map digitToInt numStr

-- | Find the maximum product of n consecutive digits in the above number
maxProduct :: Int -> Int
maxProduct n = maximum . map product $ fixedLengthSublists n digitList

-- Problem #9

-- | Generate Pythagorean triplets whose sum equals n
pythagoreanTriplets :: Int -> [[Int]]
pythagoreanTriplets n =
    [[a, b, c] | c <- [5 .. n], b <- [2 .. c - 1], a <- [1 .. b - 1], a + b + c == n, a ^ 2 + b ^ 2 == c ^ 2]

pythagoreanTripletProduct :: Int -> Int
pythagoreanTripletProduct = product . head . pythagoreanTriplets

-- Problem #10

-- | Compute the sum of all the primes below the given threshold
sumOfPrimesBelow :: Int -> Int
sumOfPrimesBelow n = sum . takeWhile (< n) $ primes

-- Construct the list of the solvers

funcList1 :: [(Int, Int -> Int)]
funcList1 =
    [ (1, sumOfMultiplesOf3Or5Below)
    , (2, sumOfEvenFibsBelow)
    , (3, largestPrimeFactor)
    , (4, largestPalindrome)
    , (5, smallestMultiple)
    , (6, sumSquareDifference)
    , (7, nthPrimeNumber)
    , (8, maxProduct)
    , (9, pythagoreanTripletProduct)
    , (10, sumOfPrimesBelow)
    ]
