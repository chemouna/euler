module Euler112 where

import Data.Monoid
import Data.Foldable
import Data.List

-- Find the least number for which the proportion of bouncy numbers is exactly 99%.
-- cannot be any bouncy numbers below one-hundred

-- bouncy = not increasing & not decreasing
-- how to calculate percentage of occurence of something in a list

isBouncy :: Int -> Bool
isBouncy n = not (isDecreasing n || isIncreasing n)

compareDigitsBy :: (Char -> Char -> Bool) -> Int -> Bool
compareDigitsBy f n = and $ zipWith f digits $ tail digits
    where digits = show n

isIncreasing :: Int -> Bool
isIncreasing = compareDigitsBy (<=)

isDecreasing :: Int -> Bool
isDecreasing = compareDigitsBy (>=)

-- euler112 = getFirst . foldMap (\c x  -> (if (isBouncy x) then c + 1  else Nothing))

getFirstPercentageBouncy :: Int -> Int -> Int -> Int
getFirstPercentageBouncy percent bouncy nonBouncy
  | done = current - 1
  | isBouncy current = getFirstPercentageBouncy percent (bouncy + 1) nonBouncy
  | otherwise = getFirstPercentageBouncy percent bouncy (nonBouncy + 1)
    where done = percent * nonBouncy == bouncy
          current = bouncy + nonBouncy + 1

euler112 = print $ getFirstPercentageBouncy 99 0 100

-- result: 1587000
ss
