module D6MemoryAllocationTest
  ( d6Tests
  ) where

import Test.HUnit

import Common
import D6MemoryAllocation (getIncrements, memAlloc, cycleLen)


incrementTests :: [TestDefinition Int [Int]]
incrementTests =
  [ TD "Basic" "" 7 [2, 2, 2, 1]
  , TD "Basic" "" 4 [1, 1, 1, 1]
  , TD "Basic" "" 3 [1, 1, 1, 0]
  ]


distributions :: [[Int]]
distributions =
  [ [2, 4, 1, 2]
  , [3, 1, 2, 3]
  , [0, 2, 3, 4]
  , [1, 3, 4, 1]
  , [2, 4, 1, 2]
  ]

memAllocTests' :: [TestDefinition [Int] [[Int]]]
memAllocTests' =
  [ TD "Basic" "" [0, 2, 7, 0] distributions
  ]


cycleTests :: [TestDefinition [[Int]] Int]
cycleTests =
  [ TD "Basic" "" distributions 4
  ]

d6Tests :: [Test]
d6Tests = fmap (apply $ getIncrements 4) incrementTests
  ++ fmap (apply memAlloc) memAllocTests'
  ++ fmap (apply cycleLen) cycleTests
