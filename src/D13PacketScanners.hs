module D13PacketScanners
  ( d13p1
  , d13p2
  , whenAtFirst
  , severity
  ) where

import Data.List.Split (splitOn)
import Data.List


whenAtFirst :: [Int] -> [(Int, Int)]-> [(Int, Int)]
whenAtFirst _ [] = []
whenAtFirst (y:ys)xs'@((xk, xv):xs)
  | xk > y = (xv, maxBound) : whenAtFirst ys xs'
  | xk == y = (xv, 2 * (xv - 1)) : whenAtFirst ys xs
  | otherwise = error "Impossible state"


severity :: [Int] -> [(Int, Int)] -> Int
severity _ [] = 0
severity (x:xs) ((depth, atFirst):ys)
  | x `mod` atFirst == 0 = x * depth + severity xs ys
  | otherwise = severity xs ys


-- Part 1

d13p1 :: String -> Int
d13p1 = severity [0..] . whenAtFirst [0..] . fmap ((\[x, y] -> (read x, read y)) . splitOn ": ") . lines


-- Part 2


-- Brutally Ineffecient (1 min 25.30 sec on this tiny input Ineffecient)
f :: [(Int, Int)] -> Int
f xs =
  let
    vals = snd <$> whenAtFirst [0..] xs
    atFirst n = True : replicate (n - 1) False
    ivals = fmap (uncurry drop) $ zip [0..] $ fmap (cycle . atFirst) vals
    tivals = transpose ivals
 in
    length $ takeWhile (not . all (== False)) tivals



d13p2 :: String -> Int
d13p2 = f . fmap ((\[x, y] -> (read x, read y)) . splitOn ": ") . lines
