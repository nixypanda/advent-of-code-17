module D12DigitalPlumber
  ( d12p1
  , d12p2
  , toMap
  , getGroupFor
  ) where


import Control.Arrow ((&&&))
import Data.List.Split (splitOn)

import qualified Data.Map as M
import qualified Data.Set as S



getGroupFor' :: S.Set Int -> Int -> M.Map Int [Int] -> [Int]
getGroupFor' visited node graph
  | null inSet = [node]
  | otherwise = node : concatMap (\x -> getGroupFor' (x `S.insert` visited) x graph) inSet
  where
    neighbours = graph M.! node
    inSet = filter (not . (`S.member` visited)) neighbours


getGroupFor :: Int -> M.Map Int [Int] -> S.Set Int
getGroupFor n = S.fromList . getGroupFor' S.empty n


toMap :: String -> M.Map Int [Int]
toMap = M.fromList
  . fmap (\(k:vs) -> (read k, read $ "[" ++ head vs ++ "]"))
  . fmap (splitOn " <-> ")
  . lines


-- Part 1

d12p1 :: String -> Int
d12p1 = S.size . getGroupFor 0 . toMap


-- Part 2

d12p2 :: String -> Int
d12p2 =
  let
    getAllGroups m = fmap (`getGroupFor` m)
  in
    S.size . S.fromList . uncurry getAllGroups . (id &&& M.keys) . toMap
