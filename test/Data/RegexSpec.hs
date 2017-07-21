module Data.RegexSpec where

import Test.Hspec
import Data.Regex

result :: Regex -> String -> Bool
result = matches

spec :: Spec
spec =
  describe "Data.Regex" $ do
    describe "derivativeString" $ do
      describe "Error" $
        it "returns Error" $
          derivativeString Error "" `shouldBe` Error

      describe "Accepting" $ do
        it "returns Accepting for empty string" $
          derivativeString (Accepting "") "" `shouldBe` Accepting ""

        it "returns Error for non empty string" $
          derivativeString (Accepting "") "a" `shouldBe` Error

      describe "Alternation" $ do
        it "returns original regex for empty string" $
          derivativeString (Alternation (Character 'a') (Character 'b')) "" `shouldBe` Alternation (Character 'a') (Character 'b')

        it "returns Accepting when left regex matches" $
          derivativeString (Alternation (Character 'a') (Character 'b')) "a" `shouldBe` Accepting "a"

        it "returns Accepting when right regex matches" $
          derivativeString (Alternation (Character 'a') (Character 'b')) "b" `shouldBe` Accepting "b"

      describe "Character" $ do
        it "returns original regex for empty string" $
          derivativeString (Character 'a') "" `shouldBe` Character 'a'

        it "returns Accepting for single character match" $
          derivativeString (Character 'a') "a" `shouldBe` Accepting "a"

        it "returns Error for single character mismatch" $
          derivativeString (Character 'a') "b" `shouldBe` Error

      describe "Optional" $ do
        it "returns original regex for empty string" $
          derivativeString (Optional (Character 'a')) "" `shouldBe` Optional (Character 'a')

        it "returns Accepting for match" $
          derivativeString (Optional (Character 'a')) "a" `shouldBe` Accepting "a"

        it "returns Error for mismatch" $
          derivativeString (Optional (Character 'a')) "b" `shouldBe` Error

      describe "Sequence" $ do
        it "returns original regex for empty string" $
          derivativeString (Sequence (Character 'a') (Character 'b')) "" `shouldBe` Sequence (Character 'a') (Character 'b')

        it "returns Accepting for match" $
          derivativeString (Sequence (Character 'a') (Character 'b')) "ab" `shouldBe` Accepting "ab"

        it "returns Error for mismatch" $ do
          derivativeString (Sequence (Character 'a') (Character 'b')) "ac" `shouldBe` Error
          derivativeString (Sequence (Character 'a') (Character 'b')) "cb" `shouldBe` Error

      describe "Not" $ do
        it "returns original regex for empty string" $
          derivativeString (Not (Character 'a')) "" `shouldBe` Not (Character 'a')

        it "returns Accepting for match" $ do
          derivativeString (Not (Character 'a')) "b" `shouldBe` Accepting "b"
          derivativeString (Not (Sequence (Character 'a') (Character 'b'))) "b" `shouldBe` Accepting "b"

        it "returns Error for mismatch" $ do
          derivativeString (Not (Character 'a')) "a" `shouldBe` Error
          derivativeString (Not (Sequence (Character 'a') (Character 'b'))) "ab" `shouldBe` Error

      describe "Repitition" $ do
        it "returns original regex for empty string" $
          derivativeString (Repetition (Character 'a')) "" `shouldBe` Repetition (Character 'a')

        it "returns itself for match" $
          derivativeString (Repetition (Character 'a')) "a" `shouldBe` Repetition (Character 'a')

        it "returns Error for mismatch" $
          derivativeString (Repetition (Character 'a')) "ab" `shouldBe` Error

    describe "matches" $ do
      it "returns True for valid matches" $ do
        result (Accepting "") "" `shouldBe` True
        result (Character 'a') "a" `shouldBe` True
        result (Sequence (Character 'a') (Character 'b')) "ab" `shouldBe` True
        result (Optional (Character 'a')) "" `shouldBe` True
        result (Optional (Character 'a')) "a" `shouldBe` True
        result (Not (Character 'a')) "b" `shouldBe` True
        result (Sequence (Repetition (Character 'a')) (Character 'c')) "aaac" `shouldBe` False
        result (Sequence (Repetition (Character 'a')) (Sequence (Repetition (Character 'b')) (Character 'c'))) "aaabbbc" `shouldBe` False

      it "returns False for invalid matches" $ do
        result (Accepting "") "a" `shouldBe` False
        result (Character 'a') "" `shouldBe` False
        result (Sequence (Character 'a') (Character 'b')) "ba" `shouldBe` False
        result (Optional (Character 'a')) "b" `shouldBe` False
        result (Not (Character 'a')) "a" `shouldBe` False
        result (Sequence (Repetition (Character 'a')) (Character 'c')) "aaad" `shouldBe` False
        result (Sequence (Repetition (Character 'a')) (Sequence (Repetition (Character 'b')) (Character 'c'))) "aaabbbd" `shouldBe` False

    describe "anaRegex" $ do
      it "returns Error for empty string" $
        anaRegex "" `shouldBe` Error

      it "returns Error for single *, ? and |" $ do
        anaRegex "*" `shouldBe` Error
        anaRegex "?" `shouldBe` Error
        anaRegex "|" `shouldBe` Error

      it "returns a Character for single character string" $
        anaRegex "a" `shouldBe` Character 'a'

      it "returns a Sequence of two single Character regexes" $
        anaRegex "ab" `shouldBe` Sequence (Character 'a') (Character 'b')

      it "returns a Sequence of Sequence for three single Character regexes" $
        anaRegex "abc" `shouldBe` Sequence (Character 'a') (Sequence (Character 'b') (Character 'c'))

      it "returns a Repetition for single character string" $
        anaRegex "a*" `shouldBe` Repetition (Character 'a')

      it "returns a Repetition for three character string" $
        anaRegex "ab*" `shouldBe` Sequence (Character 'a') (Repetition (Character 'b'))

      it "returns a Repetition for four character string" $
        anaRegex "abc*" `shouldBe` Sequence (Character 'a') (Sequence (Character 'b') (Repetition (Character 'c')))
