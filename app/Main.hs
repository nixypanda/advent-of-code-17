module Main where

import Lib (getResult)

main :: IO ()
main = do
  n <- read <$> getLine
  contents <- getLine
  putStrLn $ getResult n contents
