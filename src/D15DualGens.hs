module D15DualGens
  ( d15p1
  , d15p2
  , sameLowest16
  ) where


import Data.Bits ((.&.), complement, xor)


sameLowest16 :: Int -> Int -> Bool
sameLowest16 a b = (complement b `xor` a) .&. 65535 == 65535

xGen x = x' : xGen x' where x' = (16807 * x) `mod` 2147483647
yGen y = y' : yGen y' where y' = (48271 * y) `mod` 2147483647


d15p1 :: String -> Int
d15p1 = foldr (\x acc -> if uncurry sameLowest16 x then acc + 1 else acc) 0
  . take 40000000
  . uncurry zip
  . (\[x, y] -> (xGen $ read x, yGen $ read y))
  . lines


d15p2 :: String -> Int
d15p2 = foldr (\x acc -> if uncurry sameLowest16 x then acc + 1 else acc) 0
  . take 5000000
  . uncurry zip
  . (\[x, y] -> ( filter ((== 0) . (`mod` 4)) $ xGen $ read x
                , filter ((== 0) . (`mod` 8)) $ yGen $ read y))
  . lines
