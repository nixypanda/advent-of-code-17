module D13PacketScannersTest
  ( d13Tests
  ) where


import Test.HUnit
import Common

import D13PacketScanners (d13p1, d13p2, whenAtFirst, severity)


inp :: String
inp = unlines
  [ "0: 3"
  , "1: 2"
  , "4: 4"
  , "6: 4"
  ]


d13p1Tests :: [TestDefinition String Int]
d13p1Tests =
  [ TD "" "" inp 24
  ]


whenAtFirstTests :: [TestDefinition [(Int, Int)] [(Int, Int)]]
whenAtFirstTests =
  [ TD "" "" [(0,3),(1,2),(4,4),(6,4)] [(3,4),(2,2),(4,maxBound),(4,maxBound),(4,6),(4,maxBound),(4,6)]
  ]


severityTests :: [TestDefinition [(Int, Int)] Int]
severityTests =
  [ TD "" "" [(3,4),(2,2),(4,7),(4,7),(4,6),(4,7),(4,6)] 24
  ]


d13p2Tests :: [TestDefinition String Int]
d13p2Tests =
  [ TD "" "" inp 10
  ]


d13Tests :: [Test]
d13Tests = fmap (apply d13p1) d13p1Tests
  ++ fmap (apply $ whenAtFirst [0..]) whenAtFirstTests
  ++ fmap (apply $ severity [0..]) severityTests
  ++ fmap (apply d13p2) d13p2Tests
