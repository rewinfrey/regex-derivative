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

      describe "Accepting" $ do
        it "returns Accepting for empty string" $
          derivativeString Accepting "" `shouldBe` Accepting

        it "returns Error for non empty string" $
          derivativeString Accepting "a" `shouldBe` Error
