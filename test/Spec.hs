import Test.HUnit


import D1InverseCaptchaTest (invCaptchaTests, splitCaptchaTests)
import D2CorruptionChecksumTest (corruptionTest)


allTests :: Test
allTests = TestList
  $  invCaptchaTests
  ++ splitCaptchaTests
  ++ corruptionTest


main :: IO Counts
main =
  runTestTT allTests
