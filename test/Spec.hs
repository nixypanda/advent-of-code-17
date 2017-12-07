import Test.HUnit


import D1InverseCaptchaTest (invCaptchaTests, splitCaptchaTests)


allTests :: Test
allTests = TestList
  $  invCaptchaTests
  ++ splitCaptchaTests


main :: IO Counts
main =
  runTestTT allTests
