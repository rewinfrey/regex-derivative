module Data.RegexSpec where

import Test.Hspec
import Data.Regex

result :: Regex -> String -> Bool

spec :: Spec
spec =
  describe "Data.Regex" $

    describe "derivativeString" $ do

      describe "Error" $
        it "returns Error" $
          derivativeString Error "" `shouldBe` Error


