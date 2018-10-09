module Utils where

-- | Compute the largest integer smaller than or equal to a given integer
iSqrt :: Int -> Int
iSqrt = floor . sqrt . fromIntegral

-- | Check if a given number n is prime
-- n is a prime number if it is not divisible by any prime number in the range 2 .. [sqrt(n)]
-- we use memoization to facilitate the divisibility check
isPrime :: Int -> Bool
isPrime = (map isPrime' [0 ..] !!)
  where
    isPrime' 0 = False
    isPrime' 1 = False
    isPrime' 2 = True
    isPrime' n = all (\i -> n `mod` i /= 0) $ filter isPrime [2 .. iSqrt n]

-- | Get the sublist in the range [from, to) from a given list
-- Similar to Python's list[from:to]
slice :: Int -> Int -> [a] -> [a]
slice from to = take (to - from) . drop from

-- | Get all sublists of length n from a given list
fixedLengthSublists :: Int -> [a] -> [[a]]
fixedLengthSublists 0 _  = [[]]
fixedLengthSublists n xs = uncurry (scanl (\s x -> drop 1 s ++ [x])) $ splitAt n xs
