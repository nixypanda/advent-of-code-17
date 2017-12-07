module Lib
    ( getResult
    ) where

import D1InverseCaptcha (invCaptcha, splitCaptcha)


getResult :: Int -> String -> String
getResult 1 contents = show (invCaptcha contents) ++ "\n" ++ show (splitCaptcha contents)
