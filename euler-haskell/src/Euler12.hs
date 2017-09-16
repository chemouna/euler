module Euler12 where

import Data.List (group)

primes = 2 : filter (null . tail . primeFactors) [3,5..]

primeFactors n = factor n primes
  where
    factor n (p:ps)
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      =     factor n ps


euler12 = head $ filter ((> 500) . nDivisors) triangleNumbers
            where nDivisors n = product $ map ((+1) . length) (group (primeFactors n))
                  triangleNumbers = scanl1 (+) [1..]

euler12_sol2 = head $ filter ((> 500) . nDivisors) triangleNumbers
            where nDivisors = product . map ((+1) . length) . group . primeFactors
                  triangleNumbers = scanl1 (+) [1..]
 
