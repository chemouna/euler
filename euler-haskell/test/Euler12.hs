module Euler12Test where

import Test.QuickCheck
import Test.Hspec

import Euler12

prop_triangle_numbers (Positive n) =
  triangleNumbers !! (fromInteger n - 1) == sum [1..n]


main :: IO ()
main = hspec $ do

  describe "euler12" $ do

    it "gives 76576500 as a solution" $ do
      euler12 `shouldBe` 76576500
