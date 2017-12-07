import Test.HUnit


import D1InverseCaptchaTest (invCaptchaTests, splitCaptchaTests)
import D2CorruptionChecksumTest (corruptionTest)
import D3SpiralMemoryTest (spiralTest, stressTest')


allTests :: Test
allTests = TestList
  $  invCaptchaTests
  ++ splitCaptchaTests
  ++ corruptionTest
  ++ spiralTest
  ++ stressTest'


main :: IO Counts
main =
  runTestTT allTests
