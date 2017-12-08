module D6MemoryAllocationTest
  ( memAllocTests
  ) where

import Test.HUnit

import D6MemoryAllocation (getIncrements, memAlloc, cycleLen)


incrementTests :: [Test]
incrementTests =
  [ TestLabel "Basic" $ TestCase $ assertEqual "" [2, 2, 2, 1] $ getIncrements 4 7
  , TestLabel "Basic" $ TestCase $ assertEqual "" [1, 1, 1, 1] $ getIncrements 4 4
  , TestLabel "Basic" $ TestCase $ assertEqual "" [1, 1, 1, 0] $ getIncrements 4 3
  ]


distributions =
  [ [2, 4, 1, 2]
  , [3, 1, 2, 3]
  , [0, 2, 3, 4]
  , [1, 3, 4, 1]
  , [2, 4, 1, 2]
  ]

memAllocTests' :: [Test]
memAllocTests' =
  [ TestLabel "Basic" $ TestCase $ assertEqual "" distributions $ memAlloc [0, 2, 7, 0]
  ]


cycleTests :: [Test]
cycleTests =
  [ TestLabel "Basic" $ TestCase $ assertEqual "" 4 $ cycleLen distributions
  ]

memAllocTests :: [Test]
memAllocTests = incrementTests ++ memAllocTests' ++ cycleTests
