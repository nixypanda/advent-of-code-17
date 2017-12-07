module Lib
    ( getResult
    ) where

import D1InverseCaptcha (invCaptcha, splitCaptcha)
import D2CorruptionChecksum (checksum, evenDivision)


getResult :: Int -> String -> String
getResult 1 contents = show (invCaptcha contents) ++ "\n" ++ show (splitCaptcha contents)
getResult 2 contents = show (checksum contents) ++ "\n" ++ show (evenDivision contents)
