module Data.Regex where

import Data.Bool
import Data.List

data Regex = Error
           | Character Char
           | Sequence Regex Regex
  deriving (Show, Eq)

derivativeChar :: Regex -> Char -> Regex
derivativeChar Error _ = Error

isError :: Regex -> Bool
isError Error = True

isAccepting :: Regex -> Bool
isAccepting Error = False

derivativeString :: Regex -> String -> Regex
derivativeString regex input = foldl derivativeChar regex input

matches :: Regex -> String -> Bool
matches regex input = isAccepting $ derivativeString regex input
