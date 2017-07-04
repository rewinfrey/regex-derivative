module Main where

import Data.Regex
import Data.Semigroup ((<>))
import Data.Function (fix)

main :: IO ()
main = do
  putStrLn "Enter a character:"
  fix continue
  where
    continue loop = do
      input <- getChar
      putStrLn $ ": " ++ (show $ match (Character 'i') input)
      loop
