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

    it "parses the empty regex" $ do
      result Empty "" `shouldBe` True
      result Empty "a" `shouldBe` False

    it "parses an unmatchable regex" $ do
      result Unmatchable "" `shouldBe` False
      result Unmatchable "a" `shouldBe` False

    it "parses a sequence of character regexes" $ do
      result (Sequence (Character 'a') (Character 'b')) "ab" `shouldBe` True

