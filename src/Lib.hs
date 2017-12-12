module Lib
    ( getResult
    ) where

import D1InverseCaptcha (d1p1, d1p2)
import D2CorruptionChecksum (d2p1, d2p2)
import D3SpiralMemory (d3p1, d3p2)
import D4HighEntropyPassphrases (d4p1, d4p2)
import D5Maze (d5p1, d5p2)
import D6MemoryAllocation (d6p1, d6p2)
import D7RecursiveCircus (d7p1, d7p2)
import D8Registers (d8p1, d8p2)
import D9StreamProcessing (d9p1, d9p2)
import D10KnotHash (d10p1, d10p2)
import D11HexEd (d11p1, d11p2)
import D12DigitalPlumber (d12p1, d12p2)


answer :: (Read a, Read c, Show b, Show d) => (a -> b) -> (c -> d) -> String -> String
answer f g input = show (f (read input)) ++ "\n" ++ show (g (read input))


answer' :: (Show b, Show d) => (String -> b) -> (String -> d) -> String -> String
answer' f g input = show (f input) ++ "\n" ++ show (g input)


getResult :: Int -> String -> String
getResult  1 = answer' d1p1  d1p2
getResult  2 = answer' d2p1  d2p2
getResult  3 = answer  d3p1  d3p2
getResult  4 = answer' d4p1  d4p2
getResult  5 = answer' d5p1  d5p2
getResult  6 = answer' d6p1  d6p2
getResult  7 = answer' d7p1  d7p2
getResult  8 = answer' d8p1  d8p2
getResult  9 = answer' d9p1  d9p2
getResult 10 = answer' d10p1 d10p2
getResult 11 = answer' d11p1 d11p2
getResult 12 = answer' d12p1 d12p2
