module Data.Regex where

import Data.Bool

data Regex = Unmatchable
           | Empty
           | Symbol Char
           | Character Char
           | Alternation Regex Regex
           | Sequence Regex Regex
           | Repetition Regex
           deriving (Show, Eq)

match :: Regex -> Char -> Bool
match regex input = case regex of
  Character char -> char == input
  _ -> False
