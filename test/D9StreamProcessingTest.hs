module D9StreamProcessingTest
  ( d9Tests
  ) where

import Test.HUnit

import qualified Data.Map as M

import Common
import D9StreamProcessing (d9p1, d9p2)


d9p1Test :: [TestDefinition String Int]
d9p1Test =
  [ TD "" "" "{}" 1
  , TD "" "" "{{{}}}" 6
  , TD "" "" "{{},{}}" 5
  , TD "" "" "{{{},{},{{}}}}" 16
  , TD "" "" "{<a>,<a>,<a>,<a>}" 1
  , TD "" "" "{{<ab>},{<ab>},{<ab>},{<ab>}}" 9
  , TD "" "" "{{<!!>},{<!!>},{<!!>},{<!!>}}" 9
  , TD "" "" "{{<a!>},{<a!>},{<a!>},{<ab>}}" 3
  ]

d9p2Test :: [TestDefinition String Int]
d9p2Test =
	[ TD "" "" "<>" 0
	, TD "" "" "<random characters>" 17
	, TD "" "" "<<<<>" 3
	, TD "" "" "<{!>}>" 2
	, TD "" "" "<!!>" 0
	, TD "" "" "<!!!>>" 0
	, TD "" "" "<{o\"i!a,<{i<a>" 10
 	]

d9Tests :: [Test]
d9Tests = fmap (apply d9p1) d9p1Test
	++ fmap (apply d9p2) d9p2Test

