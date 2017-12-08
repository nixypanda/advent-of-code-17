import Test.HUnit


import D1InverseCaptchaTest (invCaptchaTests, splitCaptchaTests)
import D2CorruptionChecksumTest (corruptionTest)
import D3SpiralMemoryTest (spiralTest, stressTest')
import D4HighEntropyPassphrasesTest (validPassphraseTest, anagramTest, notAnagramTest)
import D5MazeTest (howManyStepsTest)
import D6MemoryAllocationTest (memAllocTests)
import D7RecursiveCircusTest (recTests)


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
  ++ memAllocTests
  ++ recTests


main :: IO Counts
main =
  runTestTT allTests
