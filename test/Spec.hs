import Test.HUnit


import D1InverseCaptchaTest (d1Tests)
import D2CorruptionChecksumTest (d2Tests)
import D3SpiralMemoryTest (d3Tests)
import D4HighEntropyPassphrasesTest (d4Tests)
import D5MazeTest (d5Tests)
import D6MemoryAllocationTest (d6Tests)
import D7RecursiveCircusTest (d7Tests)
import D8RegistersTest (d8Tests)
import D9StreamProcessingTest (d9Tests)
import D10KnotHashTest (d10Tests)
import D11HexEdTest (d11Tests)


allTests :: Test
allTests = TestList
  $  d1Tests
  ++ d2Tests
  ++ d3Tests
  ++ d4Tests
  ++ d5Tests
  ++ d6Tests
  ++ d7Tests
  ++ d8Tests
  ++ d9Tests
  ++ d10Tests
  ++ d11Tests


main :: IO Counts
main =
  runTestTT allTests
