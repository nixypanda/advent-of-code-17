module D2CorruptionChecksumTest
  ( d2Tests
  ) where

import Test.HUnit

import Common
import D2CorruptionChecksum (d2p1, d2p2)


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

d2p1Tests :: [TestDefinition String Int]
d2p1Tests =
  [ TD "" "" check 18
  ]


d2p2Tests :: [TestDefinition String Int]
d2p2Tests =
  [ TD "" "" evenDiv 9
  ]

d2Tests :: [Test]
d2Tests = fmap (apply d2p1) d2p1Tests ++ fmap (apply d2p2) d2p2Tests
