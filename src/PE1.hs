module PE1
    ( funcList1
    ) where

sumOfMultiplesOf3Or5Below :: Int -> Int
sumOfMultiplesOf3Or5Below x = sum . filter isMultipleOf3Or5 $ [1 .. x - 1]
  where
    isMultipleOf3Or5 t = t `mod` 3 == 0 || t `mod` 5 == 0

funcList1 :: [(Int, Int -> Int)]
funcList1 = [(1, sumOfMultiplesOf3Or5Below)]
