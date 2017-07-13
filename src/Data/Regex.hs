module Data.Regex where

import Data.Bool
import Data.List

data Regex = Error
           | Accepting
           | Character Char
           | Optional Regex
           | Sequence Regex Regex
  deriving (Show, Eq)

derivativeChar :: Regex -> Char -> Regex
derivativeChar Error _ = Error
derivativeChar Accepting _ = Error
derivativeChar (Character c) char | c == char = Accepting
                                  | otherwise = Error
derivativeChar (Optional regex) char = derivativeChar regex char

isError :: Regex -> Bool
isError Error = True
isError _ = False

isAccepting :: Regex -> Bool
isAccepting Accepting = True
isAccepting Error = False
isAccepting (Character _) = False
isAccepting (Optional _) = True

derivativeString :: Regex -> String -> Regex
derivativeString regex input = foldl derivativeChar regex input

matches :: Regex -> String -> Bool
matches regex input = isAccepting $ derivativeString regex input
