module D2CorruptionChecksum
  ( d2p1
  , d2p2
  ) where


getData :: (Read a) => String -> [[a]]
getData = fmap (fmap read . words) . lines


-- Part1

maxMinDiff :: (Num a, Ord a) => [a] -> a
maxMinDiff xs = maximum xs - minimum xs


d2p1 :: String -> Int
d2p1 = sum . fmap maxMinDiff . getData


-- Part 2

getPairSum :: [Int] -> Int
getPairSum xs =
  let
    notSame x y = x /= y
    divides x y = x `mod` y == 0
  in
    uncurry div . head . filter (\(x, y) -> notSame x y && divides x y) $ (,) <$> xs <*> xs


d2p2 :: String -> Int
d2p2 = sum . fmap getPairSum . getData

