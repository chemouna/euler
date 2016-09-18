module Euler30 where


digs :: Integer -> [Integer]
digs 0 = []
digs x = (x `mod` 10) : digs (x `div` 10)

-- to find the limit of numbers equal to the sum of 5-th powers of their decimal digits.
-- Since this sum is <= 9^5*d for a d-digit number n >= 10^(d-1),
-- there cannot be such a number with more than 6 digits. -> upperbound is 999999

sumPowersDigits :: [Integer]
sumPowersDigits = [x | x <- [2..999999], sum (map (^5) (digs x)) == x]
