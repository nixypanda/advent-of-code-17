module D15DualGensTest
  ( d15Tests
  ) where


import Test.HUnit
import Common

import D15DualGens (d15p1, sameLowest16)


sameLowest16Test :: [TestDefinition (Int, Int) Bool]
sameLowest16Test =
  [ TD "" "" (1092455, 430625591) False
  , TD "" "" (1181022009, 1233683848) False
  , TD "" "" (245556042, 1431495498) True
  , TD "" "" (1744312007, 137874439) False
  , TD "" "" (1352636452, 285222916) False
  ]

d15Tests :: [Test]
d15Tests =
  fmap (apply (uncurry sameLowest16)) sameLowest16Test

