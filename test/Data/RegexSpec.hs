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

      describe "Character" $ do
        it "returns original regex for empty string" $
          derivativeString (Character 'a') "" `shouldBe` Character 'a'

        it "returns Accepting for single character match" $
          derivativeString (Character 'a') "a" `shouldBe` Accepting

        it "returns Error for single character mismatch" $
          derivativeString (Character 'a') "b" `shouldBe` Error

      describe "Optional" $ do
        it "returns original regex for empty string" $
          derivativeString (Optional (Character 'a')) "" `shouldBe` Optional (Character 'a')

        it "returns Accepting for match" $
          derivativeString (Optional (Character 'a')) "a" `shouldBe` Accepting

        it "returns Error for mismatch" $
          derivativeString (Optional (Character 'a')) "b" `shouldBe` Error

      describe "Sequence" $ do
        it "returns original regex for empty string" $
          derivativeString (Sequence (Character 'a') (Character 'b')) "" `shouldBe` Sequence (Character 'a') (Character 'b')

        it "returns Accepting for match" $
          derivativeString (Sequence (Character 'a') (Character 'b')) "ab" `shouldBe` Accepting

        it "returns Error for mismatch" $ do
          derivativeString (Sequence (Character 'a') (Character 'b')) "ac" `shouldBe` Error
          derivativeString (Sequence (Character 'a') (Character 'b')) "cb" `shouldBe` Error
