module Euler75 where

pytr :: Int -> [(Bool, Int, Int, Int)]
pytr n = [(gcd a b == 1, a, b, c) |
      a <- [1..n],
      b <- [1..n],
      c <- [1..n],
      a < b && b < c, a^2 + b^2 == c^2, a+b+c <= n]

nbTriples :: Int -> Int
nbTriples limit = length $ pytr limit

nbPrimitives :: Int -> Int
nbPrimitives limit = sum $ map (\(x, _, _,_) -> if x then 1 else 0) $ pytr limit

-- this solution seems to take forever for only 1000

-- a solution that could work for 15000000
pyths :: Int -> [(Int, Int, Int)]
pyths x = [(m*m - n*n, 2*m*n, m*m + n*n) | m <- [2 .. limit], n <- [1 .. m],
                   (n + m) `mod` 2 == 1, gcd n m == 1]
          where limit = floor $ sqrt ((fromIntegral x) / 2);



