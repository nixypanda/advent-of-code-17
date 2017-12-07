module D1InverseCaptchaTest
  ( invCaptchaTests
  , splitCaptchaTests
  ) where

import Test.HUnit

import D1InverseCaptcha (invCaptcha, splitCaptcha)

invCaptchaTests :: [Test]
invCaptchaTests =
  [ TestLabel "Basic" $ TestCase $ assertEqual "(1122) should be 3" 3 $ invCaptcha "1122"
  , TestLabel "Basic" $ TestCase $ assertEqual "(1111) should be 4" 4 $ invCaptcha "1111"
  ]


splitCaptchaTests :: [Test]
splitCaptchaTests =
  [ TestLabel "Basic" $ TestCase $ assertEqual "(1212) should be 6" 6 $ splitCaptcha "1212"
  , TestLabel "Basic" $ TestCase $ assertEqual "(12131415) should be 4" 4 $ splitCaptcha "12131415"
  ]
