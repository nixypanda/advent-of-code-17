module D2CorruptionChecksum
  ( checksum
  , evenDivision
  ) where

getData :: (Read a) => String -> [[a]]
getData = fmap (fmap read . words) . lines

maxMinDiff :: (Num a, Ord a) => [a] -> a
maxMinDiff xs = maximum xs - minimum xs

checksum :: String -> Integer
checksum = sum . fmap maxMinDiff . getData


-- Part 2
getPairSum :: [Integer] -> Integer
getPairSum xs =
  let
    notSame x y = x /= y
    divides x y = x `mod` y == 0
  in
    uncurry div . head . filter (\(x, y) -> notSame x y && divides x y) $ (,) <$> xs <*> xs

evenDivision :: String -> Integer
evenDivision = sum . fmap getPairSum . getData
