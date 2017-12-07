module D3SpiralMemory
  ( spiralMemory
  , stressTest
  ) where

import Control.Arrow ((***))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)


data Direction
  = Right'
  | Up
  | Left'
  | Down
  deriving (Show)

type Coordinate = (Int, Int)


-- infinite list of coordinates
coords :: [Coordinate]
coords = spiral Right' (0, 0) 0 0


spiralMemory :: Int -> Int
spiralMemory n =
  let
    -- corresponding numbers that occupy the coordinate
    numAndCoords = zip [0..] coords
  in
    -- take numbers while the number is less than n
    -- get the last coordinate that is less than n
    -- sum the absolute value
    uncurry (+) . (abs *** abs) . snd . last . takeWhile ((< n) . fst) $ numAndCoords


-- Here we are constructing the infinite list of coordinates that form the spiral
-- The idea is quite simple keep going in a direction (e.g. Up) until you hit the
-- celing (or floor) {given by px (positive max) and nx (negative max)} then change the
-- direction (i.e. Right' -> Up -> Left' -> Down -> Right').
-- There is just a special case for Right' as we will increment (and decrement) px (and nx)
-- based on if that spiral is complete
spiral :: Direction -> Coordinate -> Int -> Int -> [Coordinate]
spiral Right' (px, nx) x y
  | px == x && nx == y = (x, y) : spiral Right' (px + 1, nx - 1) (x + 1) y
  | px == x = (x, y) : spiral Up (px, nx) x (y + 1)
  | otherwise = (x, y) : spiral Right' (px, nx) (x + 1) y
spiral Up (px, nx) x y
  | px == y = (x, y) : spiral Left' (px, nx) (x - 1) y
  | otherwise = (x, y) : spiral Up (px, nx) x (y + 1)
spiral Left' (px, nx) x y
  | nx == x = (x, y) : spiral Down (px, nx) x (y - 1)
  | otherwise = (x, y) : spiral Left' (px, nx) (x - 1) y
spiral Down (px, nx) x y
  | nx == y = (x, y) : spiral Right' (px, nx) (x + 1) y
  | otherwise = (x, y) : spiral Down (px, nx) x (y - 1)



-- Part 2

-- gets all neighbours for a given coordinate
getNeighbours :: Coordinate -> [Coordinate]
getNeighbours (x, y) =
  [ (x + 1, y)
  , (x + 1, y + 1)
  , (x, y + 1)
  , (x - 1, y + 1)
  , (x - 1, y)
  , (x - 1, y - 1)
  , (x, y - 1)
  , (x + 1, y - 1)
  ]


-- Get value from all the neighbours of a coordinate sum them together.
-- Append this value to the list
-- Add this coordinate and its value to a Map
-- Do it all over again
stress :: M.Map Coordinate Int -> [Coordinate] -> [Int]
stress m [] = [1]
stress m ((x, y):xs) =
  let
    neighbours = getNeighbours (x, y)
    value = sum $ map (fromMaybe 0 . (`M.lookup` m)) neighbours
    newMap = M.insert (x, y) value m
  in
    value : stress newMap xs


stressTest :: Int -> Int
stressTest n =
  let
    -- need special handling as the value at (0, 0) is 1
    -- i.e. it is not calculated in any way (but just is there)
    coords' = drop 1 coords
    baseMap = M.singleton (0, 0) 1
  in
    -- Drop all values less than (or equal to) n
    -- then take the first element of the remaining list
    head . dropWhile (<= n) $ 1 : stress baseMap coords'

