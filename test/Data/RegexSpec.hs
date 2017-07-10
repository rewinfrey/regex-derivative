module Data.RegexSpec where

import Test.Hspec
import Data.Regex

result :: Regex -> String -> Bool
result = match

spec :: Spec
spec =
  describe "match" $ do
    it "parses a single character regex" $ do
      result (Character 'a') "a" `shouldBe` True
      result (Character 'b') "a" `shouldBe` False

    it "parses a single symbol regex" $ do
      result (Symbol '$') "$" `shouldBe` True
      result (Symbol '%') "$" `shouldBe` False

    it "allows empty regex" $ do
      result Empty "" `shouldBe` True
      result Empty "a" `shouldBe` False

