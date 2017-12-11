module D10KnotHashTest
  ( d10Tests
  ) where

import Test.HUnit

import Common
import D10KnotHash
  ( d10p1, d10p2
  , knotHash, denseHash, sparseHash
  , zipWithL, toASCII, toHex
  )


results :: [[Int]]
results =
  [ [2, 1, 0, 3, 4]
  , [4, 3, 0, 1, 2]
  , [4, 3, 0, 1, 2]
  , [3, 4, 2, 1, 0]
  ]

knotTests :: [Test]
knotTests =
  [ TestLabel "" $ TestCase $ assertEqual "" results $ knotHash [0, 1, 2, 3, 4] [3, 4, 1, 5]
  ]


denseHashTests :: [TestDefinition String String]
denseHashTests =
  [ TD "" "" ""         "a2582a3a0e66e6e86e3812dcb672a272"
  , TD "" "" "AoC 2017" "33efeb34ea91902bb2f59c9920caa6cd"
  , TD "" "" "1,2,3"    "3efbe78a8d82f29979031a4aa0b16a9d"
  , TD "" "" "1,2,4"    "63960835bcdc130f0b66d7ff4f6a5a8e"
  ]


zipWithLTests :: [TestDefinition [[Int]] [Int]]
zipWithLTests =
  [ TD "" "" (fmap (:[]) [65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22]) [64]
  ]


toASCIITests :: [TestDefinition String [Int]]
toASCIITests =
  [ TD "" "" "1,2,3" [49,44,50,44,51,17,31,73,47,23]
  ]


toHexTests :: [TestDefinition [Int] String]
toHexTests =
  [ TD "" "" [64, 7, 255] "4007ff"
  ]


sparseHashTests :: [Test]
sparseHashTests =
  [ TestLabel "" $ TestCase $ assertEqual "" [3, 4, 2, 1, 0] $ sparseHash 1 [0, 1, 2, 3, 4] [3, 4, 1, 5]
  , TestLabel "" $ TestCase $ assertEqual "" [2, 3, 1, 0, 4] $ sparseHash 2 [0, 1, 2, 3, 4] [3, 4, 1, 5]
  ]


d10Tests :: [Test]
d10Tests = knotTests
  ++ sparseHashTests
  ++ fmap (apply zipWithL) zipWithLTests
  ++ fmap (apply toASCII) toASCIITests
  ++ fmap (apply denseHash) denseHashTests
  ++ fmap (apply toHex) toHexTests

