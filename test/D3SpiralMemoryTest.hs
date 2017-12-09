module D3SpiralMemoryTest
  ( d3Tests
  ) where

import Test.HUnit

import Common
import D3SpiralMemory (d3p1, d3p2)



d3p1Tests :: [TestDefinition Int Int]
d3p1Tests =
  [ TD "" "" 1 0
  , TD "" "" 12 3
  , TD "" "" 23 2
  , TD "" "" 1024 31
  ]


d3p2Tests :: [TestDefinition Int Int]
d3p2Tests =
  [ TD "" "" 4 5
  , TD "" "" 122 133
  , TD "" "" 747 806
  , TD "" "" 330 351
  ]


d3Tests :: [Test]
d3Tests = fmap (apply d3p1) d3p1Tests ++ fmap (apply d3p2) d3p2Tests

