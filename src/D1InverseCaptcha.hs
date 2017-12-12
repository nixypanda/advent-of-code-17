module D1InverseCaptcha
  ( d1p1
  , d1p2
  ) where

import Data.Char (digitToInt)


toDigit :: Char -> Int
toDigit = fromIntegral . digitToInt


-- Part 1

isum :: String -> Int
isum [] = 0
isum [x] = 0
isum (x:y:xs)
  | x == y = toDigit x + isum (y:xs)
  | otherwise = isum (y:xs)


append :: String -> String
append [] = []
append (x:xs) = x:xs ++ [x]


d1p1 :: String -> Int
d1p1 = isum . append


-- Part Two

sumWhenSame :: String -> String -> Int
sumWhenSame xs ys = sum $ zipWith (\x y -> if x == y then toDigit x + toDigit y else 0) xs ys


d1p2 :: String -> Int
d1p2 xs = uncurry sumWhenSame $ splitAt (length xs `div` 2) xs
