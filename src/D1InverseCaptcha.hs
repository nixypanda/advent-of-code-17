module D1InverseCaptcha
  ( invCaptcha
  , splitCaptcha
  ) where

import Data.Char (digitToInt)


toDigit :: Char -> Integer
toDigit = fromIntegral . digitToInt


isum :: String -> Integer
isum [] = 0
isum [x] = 0
isum (x:y:xs)
  | x == y = toDigit x + isum (y:xs)
  | otherwise = isum (y:xs)


append :: String -> String
append [] = []
append (x:xs) = x:xs ++ [x]

invCaptcha :: String -> Integer
invCaptcha = isum . append


-- Part Two
split :: Int -> [a] -> ([a], [a])
split n xs = (take n xs, drop n xs)

splitHalf :: [a] -> ([a], [a])
splitHalf xs = split (length xs `div` 2) xs


sumWhenSame :: String -> String -> Integer
sumWhenSame xs ys = sum $ zipWith (\x y -> if x == y then toDigit x + toDigit y else 0) xs ys

splitCaptcha :: String -> Integer
splitCaptcha = uncurry sumWhenSame . splitHalf
