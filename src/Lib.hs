module Lib
    ( getResult
    ) where

import D1InverseCaptcha (invCaptcha, splitCaptcha)
import D2CorruptionChecksum (checksum, evenDivision)
import D3SpiralMemory (spiralMemory, stressTest)
import D4HighEntropyPassphrases (areValid, notAnagrams)
import D5Maze (howManySteps, howManySteps')
import D6MemoryAllocation (sol6p1, sol6p2)
import D7RecursiveCircus (sol7p1, sol7p2)
import D8Registers (sol8p1, sol8p2)


answer :: (Read a, Read c, Show b, Show d) => (a -> b) -> (c -> d) -> String -> String
answer f g input = show (f (read input)) ++ "\n" ++ show (g (read input))


answer' :: (Show b, Show d) => (String -> b) -> (String -> d) -> String -> String
answer' f g input = show (f input) ++ "\n" ++ show (g input)


getResult :: Int -> String -> String
getResult 1 = answer' invCaptcha splitCaptcha
getResult 2 = answer' checksum evenDivision
getResult 3 = answer spiralMemory stressTest
getResult 4 = answer' areValid notAnagrams
getResult 5 = answer' howManySteps howManySteps'
getResult 6 = answer' sol6p1 sol6p2
getResult 7 = answer' sol7p1 sol7p2
getResult 8 = answer' sol8p1 sol8p2
