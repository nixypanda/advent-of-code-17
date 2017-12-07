import Test.HUnit


import D1InverseCaptchaTest (invCaptchaTests, splitCaptchaTests)
import D2CorruptionChecksumTest (corruptionTest)
import D3SpiralMemoryTest (spiralTest, stressTest')
import D4HighEntropyPassphrasesTest (validPassphraseTest, anagramTest, notAnagramTest)


allTests :: Test
allTests = TestList
  $  invCaptchaTests
  ++ splitCaptchaTests
  ++ corruptionTest
  ++ spiralTest
  ++ stressTest'
  ++ validPassphraseTest
  ++ anagramTest
  ++ notAnagramTest


main :: IO Counts
main =
  runTestTT allTests
