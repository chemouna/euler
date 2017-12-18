
module Euler81 where

import Text.CSV

readInt :: String -> Int
readInt = read

minAccRow :: [Int] -> [Int] -> [Int]
minAccRow acc = undefined

euler81 :: [[Int]] -> Int
euler81 m = last (foldl1 minAccRow (extendM m))

extendM :: [[Int]] -> [[Int]]
extendM m = [scanl1 (+) $ head m] ++ tail m

main = do
  input <- parseCSVFromFile "euler81_matrix_small.txt"
  let
    Right c = input
    matrix = map (map readInt) c
  putStrLn $ show (euler81 matrix)
  return ()
