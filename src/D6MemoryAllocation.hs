module D6MemoryAllocation
  ( getIncrements
  , memAlloc
  , cycleLen
  , d6p1
  , d6p2
  ) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

import qualified Data.Set as S


-- Honestly this one feels weird to solve using Pure FP (or I need to level
-- up my FP foo)

-- Part 1


-- Gives a list of values by which we will increment the original list
-- after the max point
getIncrements :: Int -> Int -> [Int]
getIncrements len bank =
  let
    getIncrements' :: Int -> Int -> [Int] -> [Int]
    getIncrements' len bank xs
      | bank >= len = getIncrements' len (bank - len) (fmap (+1) xs)
      | otherwise = zipWith (+) (replicate bank 1 ++ repeat 0) xs
  in
    getIncrements' len bank (replicate len 0)


memAlloc' :: S.Set [Int] -> Int -> [Int] -> [[Int]]
memAlloc' set len xs =
  let
    maxX = maximum xs
    -- construct a new list from old one by
    -- 1. depleting the max value
    -- 2. rotating it to after max value
    xs' = init (take len $ tail $ dropWhile (< maxX) $ cycle xs) ++ [0]
    increments = getIncrements len maxX
    -- apply increments to the rotated list
    xs'' = zipWith (+) xs' increments

    -- re-rotate to original position (so it can be stored in a set)
    maxLoc = length $ takeWhile (< maxX) xs
    nxs = take len $ drop (len - maxLoc - 1) $ cycle xs''
    set' = S.insert nxs set
  in
    -- if we find an occurence that we have seen before stop else continue
    if S.member nxs set
       then [nxs]
       else nxs : memAlloc' set' len nxs


memAlloc :: [Int] -> [[Int]]
memAlloc xs = memAlloc' S.empty (length xs) xs


d6p1 :: String -> Int
d6p1 = length . memAlloc . fmap read . words


-- Part 2

cycleLen :: [[Int]] -> Int
cycleLen xs = length xs - fromJust (elemIndex x xs) - 1 where x = last xs


d6p2 :: String -> Int
d6p2 = cycleLen . memAlloc . fmap read . words
