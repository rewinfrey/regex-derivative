module Data.Regex where

import Data.Bool
import Data.List

data Regex = Unmatchable
           | Empty
           | Character Char
           | Alternation Regex Regex
           | Sequence Regex Regex
           | Repetition Regex
           deriving (Show, Eq)

match :: Regex -> String -> Bool
match regex [] = case regex of
  Empty -> True
  _ -> False
match regex input@(x:xs) = case regex of
  Empty -> False
  Character char -> char == x
  Sequence reg1 reg2 -> match reg1 input && match reg2 (tail input)
  _ -> False
