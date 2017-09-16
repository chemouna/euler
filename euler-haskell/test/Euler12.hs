
module Euler12Test where

import Test.QuickCheck

import Euler12

prop_triangle_numbers (Positive n) =
  triangleNumbers !! (fromInteger n - 1) == sum [1..n]

  
