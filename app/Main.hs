module Main where

import Data.Regex
import Data.Semigroup ((<>))
import Data.Function (fix)

main :: IO ()
main = do
  putStrLn "Enter input string:"
  fix continue
  where
    continue loop = do
      input <- getLine
      putStrLn (show $ match (Sequence (Character 'i') (Character 'j')) input)
      loop
