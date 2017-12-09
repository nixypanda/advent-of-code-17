module D5Maze
  ( d5p1
  , d5p2
  ) where


import qualified Data.Map as M


theMaze :: [Int] -> M.Map Int Int
theMaze xs = M.fromList $ zip [0..] xs


jump :: (Int -> Int) -> Int -> M.Map Int Int -> Int
jump incrF key maze =
  let
    val = M.lookup key maze
    maze' = M.update (Just . incrF) key maze
  in
    case val of
      (Just val') -> 1 + jump incrF (key + val') maze'
      Nothing -> 0


-- Part 1

d5p1 :: String -> Int
d5p1 = jump (+1) 0 . theMaze . fmap read . lines


-- Part 2

d5p2 :: String -> Int
d5p2 = jump (\x -> if x >= 3 then x - 1 else x + 1) 0 . theMaze . fmap read . lines
