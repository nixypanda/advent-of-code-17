module D9StreamProcessing
  ( d9p1
  , d9p2
  ) where


data Mode = Accept | Reject deriving (Show, Eq)


-- Part 1

d9p1 :: String -> Int
d9p1 = stream Accept 0


stream :: Mode -> Int -> String -> Int
stream _ _ [] = 0
stream mode n (',':xs) = stream mode n xs
stream mode n ('!':_:xs) = stream mode n xs
stream Accept n ('{':xs) = stream Accept (n + 1) xs
stream Accept n ('}':xs) = n + stream Accept (n - 1) xs
stream mode n ('<':xs) = stream Reject n xs
stream mode n ('>':xs) = stream Accept n xs
stream Reject n (_:xs) = stream Reject n xs
stream _ _ _ = error "Out of Scope :P"


-- Part 2

d9p2 :: String -> Int
d9p2 = stream' Accept


stream' :: Mode -> String -> Int
stream' _ [] = 0
stream' mode ('!':_:xs) = stream' mode xs
stream' Reject ('<':xs) = 1 + stream' Reject xs
stream' Accept ('<':xs) = stream' Reject xs
stream' mode ('>':xs) = stream' Accept xs
stream' Reject (_:xs) = 1 + stream' Reject xs
stream' Accept (_:xs) = stream' Accept xs
stream' _ _ = error "Out of Scope :P"

