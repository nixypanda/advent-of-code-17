module D2CorruptionChecksumTest
  ( corruptionTest
  ) where

import Test.HUnit

import D2CorruptionChecksum (checksum, evenDivision)


check :: String
check = unlines
  [ "5 1 9 5"
  , "7 5 3"
  , "2 4 6 8"
  ]


evenDiv :: String
evenDiv = unlines
  [ "5 9 2 8"
  , "9 4 7 3"
  , "3 8 6 5"
  ]


corruptionTest :: [Test]
corruptionTest =
  [ TestLabel "Basic" $ TestCase $ assertEqual (check ++ " should be 18") 18 $ checksum check
  , TestLabel "Basic" $ TestCase $ assertEqual (evenDiv ++ " should be 9") 9 $ evenDivision evenDiv
  ]

