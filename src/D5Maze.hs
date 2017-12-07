module D5Maze
  ( howManySteps
  , howManySteps'
  ) where


import qualified Data.Map as M


-- Part 1

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


howManySteps :: String -> Int
howManySteps = jump (+1) 0 . theMaze . fmap read . lines


-- Part 2
howManySteps' :: String -> Int
howManySteps' = jump (\x -> if x >= 3 then x - 1 else x + 1) 0 . theMaze . fmap read . lines
