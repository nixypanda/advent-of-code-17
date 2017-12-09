module D1InverseCaptchaTest
  ( d1Tests
  ) where

import Test.HUnit

import Common
import D1InverseCaptcha (d1p1, d1p2)


d1p1Tests :: [TestDefinition String Int]
d1p1Tests =
  [ TD "" "" "1122" 3
  , TD "" "" "1111" 4
  ]


d1p2Tests :: [TestDefinition String Int]
d1p2Tests =
  [ TD "" "" "1212" 6
  , TD "" "" "12131415" 4
  ]

d1Tests :: [Test]
d1Tests = fmap (apply d1p1) d1p1Tests ++ fmap (apply d1p2) d1p2Tests

