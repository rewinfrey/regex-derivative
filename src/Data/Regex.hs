module Data.Regex where

import Data.Bool
import Data.List
import Data.Set hiding (foldl)

data Regex = Error
           | Accepting
           | Alternation Regex Regex
           | Character Char
           | Optional Regex
           | Sequence Regex Regex
           | Repetition Regex
           | Not Regex
  deriving (Show, Eq)

derivativeChar :: Regex -> Char -> Regex
derivativeChar Error _ = Error
derivativeChar Accepting _ = Error
derivativeChar (Alternation regex1 regex2) char | isAccepting regex1 = Accepting
                                                | isError regex1 = derivativeChar regex2 char
                                                | otherwise = derivativeChar (Alternation (derivativeChar regex1 char) regex2) char
derivativeChar (Character c) char | c == char = Accepting
                                  | otherwise = Error
derivativeChar (Optional regex) char = derivativeChar regex char
derivativeChar (Sequence regex1 regex2) char | isAccepting regex1 = derivativeChar regex2 char
                                             | isError regex1 = Error
                                             | otherwise = Sequence (derivativeChar regex1 char) regex2
derivativeChar (Not regex) char | isAccepting (derivativeChar regex char) = Error
                                | isError (derivativeChar regex char) = Accepting
                                | otherwise = Error
derivativeChar (Repetition regex) char | isAccepting (derivativeChar regex char) = (Repetition regex)
                                       | otherwise = derivativeChar regex char

isError :: Regex -> Bool
isError Error = True
isError (Sequence r1 r2) = isError r1 || isError r2
isError _ = False

isAccepting :: Regex -> Bool
isAccepting Accepting = True
isAccepting (Optional _) = True
isAccepting (Sequence r1 r2) = isAccepting r1 && isAccepting r2
isAccepting (Alternation r1 r2) = isAccepting r1 || isAccepting r2
isAccepting (Not regex) | isAccepting regex = False
                        | isError regex = True
                        | otherwise = False
isAccepting _ = False

derivativeString :: Regex -> String -> Regex
derivativeString regex input = foldl derivativeChar regex input

matches :: Regex -> String -> Bool
matches regex input = isAccepting $ derivativeString regex input



-- Building Regex from an input string
anaRegex :: String -> Regex
anaRegex [] = Error
anaRegex [x] | isCharacter x = Character x
             | otherwise = Error
anaRegex (x:y:xs) | isRepetition y = Repetition (anaRegex [x])
                  | isOptional y = Optional (anaRegex [y])
                  | isCharacter x = Sequence (Character x) (anaRegex (y:xs))

isCharacter :: Char -> Bool
isCharacter c = not $ member c regexSymbols

isRepetition :: Char -> Bool
isRepetition c = member c regexSymbols && c == '*'

isOptional :: Char -> Bool
isOptional c = member c regexSymbols && c == '?'

regexSymbols :: Set Char
regexSymbols = fromList ['*', '?', '|']
