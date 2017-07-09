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
