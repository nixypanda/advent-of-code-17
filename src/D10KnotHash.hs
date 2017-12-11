module D10KnotHash
  ( d10p1
  , d10p2
  , knotHash
  , denseHash
  , sparseHash
  , zipWithL
  , toASCII
  , toHex
  , knotHash'
  ) where

import Data.List (foldl', unfoldr, transpose)
import Data.List.Split (splitOn, chunksOf)
import Data.Char (ord)
import Data.Bits (xor)
import Data.Bool (bool)
import Text.Printf (printf)
import Numeric (showHex)

-- Part 1

type Skip = Int
type Length = Int
type Location = Int
type Iteration = Int


d10p1 :: String -> Int
d10p1 = product . take 2 . last . knotHash [0..255] . fmap read . splitOn ","


knotHash :: [Int] -> [Int] -> [[Int]]
knotHash xs ls = (\(xss, _, _) -> xss) <$> knotHash' (length xs) 0 0 ls xs


knotHash' :: Length -> Skip -> Location -> [Int] -> [Int] -> [([Int], Skip, Location)]
knotHash' _ _ _ [] xs = []
knotHash' len skip loc (l:ls) ys =
  let
    cycle' n = take len . drop n . cycle
    ixs  = cycle $ cycle' loc ys
    xs'  = cycle' (len - loc) $ reverse (take l ixs) ++ take (len - l) (drop l ixs)
    loc' = (loc + skip + l) `mod` len
    skip' = skip + 1
  in
    (xs', skip', loc') : knotHash' len skip' loc' ls xs'


-- Part 2


toASCII :: String -> [Int]
toASCII = (++ [17, 31, 73, 47, 23]) . fmap ord


sparseHash :: Int -> [Int] -> [Int] -> [Int]
sparseHash = sparseHash' 0 0


sparseHash' :: Skip -> Location -> Iteration -> [Int] -> [Int] -> [Int]
sparseHash' _ _ 0 _ _ = error "WTF Mate"
sparseHash' skip loc 1 xs ls = (\(xs, _, _) -> xs) . last $ knotHash' (length xs) skip loc ls xs
sparseHash' skip loc t xs ls =
  let (_, skip', loc') = last $ knotHash' (length xs) skip loc ls xs
  in sparseHash' skip' loc' (t - 1) xs ls


zipWithL :: [[Int]] -> [Int]
zipWithL = fmap (foldr1 xor) . transpose


toHex :: [Int] -> String
toHex = concatMap ((\x -> if length x == 1 then '0':x else x) . (`showHex` ""))


denseHash :: String -> String
denseHash = toHex . zipWithL . chunksOf 16 . sparseHash 65 [0..255] . toASCII 


d10p2 :: String -> String
d10p2 = denseHash
