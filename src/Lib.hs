module Lib
    ( getResult
    ) where

import D1InverseCaptcha (invCaptcha, splitCaptcha)
import D2CorruptionChecksum (checksum, evenDivision)
import D3SpiralMemory (spiralMemory, stressTest)
import D4HighEntropyPassphrases (areValid, notAnagrams)
import D5Maze (howManySteps, howManySteps')


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
