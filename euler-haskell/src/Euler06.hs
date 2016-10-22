module Euler06 where

import Data.Foldable

-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

diffSquares :: Int -> Int
diffSquares n = ((sum [1..n])^2) - (sum $ map (^2) [1..n])

-- diffSquares 100 = 25164150

diffSquares2 :: Int -> Int
diffSquares2 n = (foldl1 (+) [1..n])^2 - (foldl1 (+) . map (^2) $ [1..n])
