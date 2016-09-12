module Euler193 where

import Math.NumberTheory.Primes (isPrime)
import Data.Array.Unboxed (Array, listArray, (!))
import System.CPUTime (getCPUTime)

maxN = 2^50

primesSquared :: Array Int Integer
primesSquared = listArray (0, length p2) p2
  where p2 = takeWhile (<= maxN)
             (map (^2) $ 2:[p | p <- [2,5..], isPrime p]) ++ [maxN+1]


f (result,partialProduct,i,isOdd)
  | seq result $ seq partialProduct $ seq i $ seq isOdd $ False = undefined
  | nextProduct < maxN =
      (nextPrimeSquared . descendLevel) (result+adjustCount)
  | otherwise = result+adjustCount
  where
      nextProduct        = ((primesSquared ! i) * partialProduct)
      numberOfSquareFull = maxN `div` nextProduct
      adjustCount | isOdd     = numberOfSquareFull
                  | otherwise = -numberOfSquareFull
      descendLevel x     = f (x, nextProduct,    i+1, not isOdd)
      nextPrimeSquared x = f (x, partialProduct, i+1,     isOdd)

main = do
  a <- seq primesSquared $ getCPUTime
  let b = maxN - f (0,1,0,True)
  putStrLn $ "Result = " ++ show b
  c <- getCPUTime
  putStrLn $ "Prime Calculation took " ++ show (a `div` 10^12) ++ " seconds"
  putStrLn $ "Moebius Calculation took " ++ show ((c-a) `div` 10^12)
             ++ " seconds" 

-- solution #2
muTable :: Int -> UArray Int Int
muTable 
