module D5MazeTest
  ( howManyStepsTest
  ) where

import Test.HUnit

import D5Maze (howManySteps, howManySteps')


testData = unlines $ show <$> [0, 3, 0, 1, -3]

howManyStepsTest :: [Test]
howManyStepsTest =
  [ TestLabel "Basic" $ TestCase $ assertEqual "" 5 $ howManySteps testData
  , TestLabel "Basic" $ TestCase $ assertEqual "" 10 $ howManySteps' testData
  ]

