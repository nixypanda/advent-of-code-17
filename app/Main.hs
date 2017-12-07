module Main where

import Lib (getResult)

main :: IO ()
main = do
  n <- read <$> getLine
  -- init to remove trailing `\n`
  contents <- init <$> getContents
  putStrLn $ getResult n contents
