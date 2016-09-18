
module Euler30 where


digs :: Integer -> [Integer]
digs 0 = []
digs x = (x `mod` 10) : digs (x `div` 10)

sumPowersDigits :: Integer
sumPowersDigits = sum [x | x <- [2..500000], sum (map (^5) (digs x)) == x]


