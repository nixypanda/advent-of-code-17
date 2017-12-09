module D7RecursiveCircus
  ( Rose(..)
  , getHead
  , readMap
  , stringToRose
  , d7p1
  , getInvalidNode
  , d7p2
  ) where

import Data.Maybe (fromJust)
import Control.Arrow ((&&&))
import Data.List (groupBy)

import qualified Data.Set as S
import qualified Data.Map as M


-- Part 1

getHead :: [(String, Int, [String])] -> String
getHead xs =
  let
    nodes = S.fromList $ fmap (\(x, _, _) -> x) xs
    neighbours = S.fromList $ concatMap (\(_, _, x) -> x) xs
  in
    head . S.toList $ S.difference nodes neighbours


readMap :: String -> [(String, Int, [String])]
readMap =
  let
    toInt = read . init . tail
    toList [] = []
    toList ("->":rest) = map (filter (/= ',')) rest
  in
    fmap ((\(node:weight:others) -> (node, toInt weight, toList others)) . words) . lines


d7p1 :: String -> String
d7p1 = getHead . readMap


-- Part 2
-- Ok this dution needs some cleanup IMO it's going crazy here

toMap :: [(String, Int, [String])] -> M.Map String (Int, [String])
toMap xs = M.fromList [(x, (y, z)) | (x, y, z) <- xs]


data Rose k v =
  Rose k v [Rose k v]
  deriving (Show, Eq)


getWeight :: Rose String Int -> Int
getWeight (Rose _ v _) = v

getKey :: Rose String Int -> String
getKey (Rose k _ _) = k


toRose :: M.Map String (Int, [String]) -> String -> Rose String Int
toRose m node =
  let
    (weight, children) = fromJust $ M.lookup node m
  in
    Rose node weight $ fmap (toRose m) children


weightedRose :: Rose String Int -> Rose String Int
weightedRose (Rose k v []) = Rose k v []
weightedRose (Rose k v xs) = Rose k (sum $ v : fmap getWeight res) res
  where res = fmap weightedRose xs


getInvalidNode :: Rose String Int -> String
getInvalidNode r@(Rose k v []) = k
getInvalidNode r@(Rose k v rs) =
  let
    xs = groupBy (\x y -> getWeight x == getWeight y) rs
    oddOne = head . head $ filter ((== 1) . length) xs
  in
    if length xs == 1
       then k
       else getInvalidNode oddOne


siblingOf :: String -> Rose String Int -> [Rose String Int]
siblingOf n (Rose k v []) = []
siblingOf n r@(Rose k v xs) =
  if n `elem` fmap getKey xs
     then xs
     else concatMap (siblingOf n) xs


stringToRose :: String -> Rose String Int
stringToRose = uncurry toRose . (toMap &&& getHead) . readMap


d7p2 :: String -> ([Int], [Int])
d7p2 s =
  let
    r = stringToRose s
    weightedR = weightedRose r
    iNode = getInvalidNode weightedR
    weights = getWeight <$> siblingOf iNode weightedR
    weights' = getWeight <$> siblingOf iNode r
  in
    (weights, weights')

