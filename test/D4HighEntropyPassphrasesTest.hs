module D4HighEntropyPassphrasesTest
  ( d4Tests
  ) where

import Test.HUnit

import Common
import D4HighEntropyPassphrases (isHighEntropy, anagrams, anyAnagrams, d4p2)


validPassphraseTests :: [TestDefinition [String] Bool]
validPassphraseTests =
  [ TD "" "" (words "aa bb cc dd ee") True
  , TD "" "" (words "aa bb cc dd aa") False
  , TD "" "" (words "aa bb cc dd aaa") True
  ]


anyAnagramsTests :: [TestDefinition [String] Bool]
anyAnagramsTests =
  [ TD "" "" ["abcde", "fghij"] False
  , TD "" "" ["abcde", "xyz", "ecdab"] True
  , TD "" "" (words "a ab abc abd abf abj") False
  ]


notAnagramTests :: [TestDefinition String Int]
notAnagramTests =
  [ TD "" "" phrases 3
  ]

d4Tests :: [Test]
d4Tests =
  fmap (apply isHighEntropy) validPassphraseTests
  ++ anagramTest
  ++ fmap (apply anyAnagrams) anyAnagramsTests
  ++ fmap (apply d4p2) notAnagramTests


anagramTest :: [Test]
anagramTest =
  [ TestLabel "Anagram" $ TestCase $ assertEqual "abcde fghij are not" False $ anagrams "abcde" "fghij"
  , TestLabel "Anagram" $ TestCase $ assertEqual "abcde ecdab are" True $ anagrams "abcde" "ecdab"
  ]


phrases :: String
phrases = unlines
  [ "abcde fghij"
  , "abcde xyz ecdab"
  , "a ab abc abd abf abj"
  , "iiii oiii ooii oooi oooo"
  , "oiii ioii iioi iiio"
  ]

