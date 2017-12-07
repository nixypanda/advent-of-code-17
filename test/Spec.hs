import Test.HUnit


import D1InverseCaptchaTest (invCaptchaTests, splitCaptchaTests)
import D2CorruptionChecksumTest (corruptionTest)
import D3SpiralMemoryTest (spiralTest, stressTest')
import D4HighEntropyPassphrasesTest (validPassphraseTest, anagramTest, notAnagramTest)
import D5MazeTest (howManyStepsTest)


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
  ++ howManyStepsTest


main :: IO Counts
main =
  runTestTT allTests
