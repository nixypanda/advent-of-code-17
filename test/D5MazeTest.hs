module D5MazeTest
  ( d5Tests
  ) where

import Test.HUnit

import Common
import D5Maze (d5p1, d5p2)


testData = unlines $ show <$> [0, 3, 0, 1, -3]


d5p1Tests :: [TestDefinition String Int]
d5p1Tests =
  [ TD "" "" testData 5
  ]


d5p2Tests :: [TestDefinition String Int]
d5p2Tests =
  [ TD "" "" testData 10
  ]

d5Tests :: [Test]
d5Tests = fmap (apply d5p1) d5p1Tests ++ fmap (apply d5p2) d5p2Tests


