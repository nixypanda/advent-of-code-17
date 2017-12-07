module D4HighEntropyPassphrases
  ( isHighEntropy
  , areValid
  , anagrams
  , anyAnagrams
  , notAnagrams
  ) where

import Control.Arrow ((&&&))
import Data.List (tails)
import qualified Data.Set as S
import qualified Data.Map as M


-- Part 1

isHighEntropy :: (Ord a) => [a] -> Bool
isHighEntropy =
  uncurry (==) . (length &&& (S.size . S.fromList))


areValid :: String -> Int
areValid =
  length . filter (== True) . fmap (isHighEntropy . words) . lines


-- Part 2

anagrams :: String -> String -> Bool
anagrams xs ys =
  toMap xs == toMap ys where toMap ls = M.fromListWith (+) [(l, 1) | l <- ls]


anyAnagrams :: [String] -> Bool
anyAnagrams =
  let
    pairs xs = [(x,y) | (x:ys) <- tails xs, y <- ys]
  in
    or . fmap (uncurry anagrams) . pairs


notAnagrams :: String -> Int
notAnagrams =
  length . filter (== False) . fmap (anyAnagrams . words) . lines
