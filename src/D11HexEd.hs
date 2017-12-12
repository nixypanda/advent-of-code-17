module D11HexEd
  ( d11p1
  , d11p2
  ) where

import Data.List.Split (splitOn)


data Direction
  = North
  | NorthEast
  | SouthEast
  | South
  | SouthWest
  | NorthWest
  deriving (Show)


type Point = (Int, Int, Int)


readDirection :: String -> Direction
readDirection "n" = North
readDirection "ne" = NorthEast
readDirection "se" = SouthEast
readDirection "s" = South
readDirection "sw" = SouthWest
readDirection "nw" = NorthWest
readDirection x = error ("Not a direction: " ++ show x)


coords :: Point -> [Direction] -> [Point]
coords p [] = []
coords p (x:xs) = p' : coords p' xs where p' = newLoc x p


newLoc :: Direction -> Point -> Point
newLoc North     (x, y, z) = (x, y + 1, z - 1)
newLoc NorthEast (x, y, z) = (x + 1, y, z - 1)
newLoc SouthEast (x, y, z) = (x + 1, y - 1, z)
newLoc South     (x, y, z) = (x, y - 1, z + 1)
newLoc SouthWest (x, y, z) = (x - 1, y, z + 1)
newLoc NorthWest (x, y, z) = (x - 1, y + 1, z)


dist :: Point -> Point -> Int
dist (x1, y1, z1) (x2, y2, z2) = maximum $ fmap abs [x2 - x1, y2 - y1, z2 - z1]


-- Part 1

d11p1 :: String -> Int
d11p1 = dist (0, 0, 0) . last . coords (0, 0, 0) . fmap readDirection . splitOn ","


-- Part 2

d11p2 :: String -> Int
d11p2 = maximum . fmap (dist (0, 0, 0)) . coords (0, 0, 0) . fmap readDirection . splitOn ","
