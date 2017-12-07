module D3SpiralMemoryTest
  ( spiralTest
  , stressTest'
  ) where

import Test.HUnit

import D3SpiralMemory (spiralMemory, stressTest)

spiralTest :: [Test]
spiralTest =
  [ TestLabel "Basic" $ TestCase $ assertEqual "1 is carried 0 steps" 0 $ spiralMemory 1
  , TestLabel "Basic" $ TestCase $ assertEqual "12 is carried 3 steps" 3 $ spiralMemory 12
  , TestLabel "Basic" $ TestCase $ assertEqual "23 is carried 2 steps" 2 $ spiralMemory 23
  , TestLabel "Basic" $ TestCase $ assertEqual "1024 is carried 31 steps" 31 $ spiralMemory 1024
  ]

stressTest' :: [Test]
stressTest' =
  [ TestLabel "Basic" $ TestCase $ assertEqual "4 < 5" 5 $ stressTest 4
  , TestLabel "Basic" $ TestCase $ assertEqual "122 < 133" 133 $ stressTest 122
  , TestLabel "Basic" $ TestCase $ assertEqual "747 < 806" 806 $ stressTest 747
  , TestLabel "Basic" $ TestCase $ assertEqual "330 < 351" 351 $ stressTest 330
  ]


