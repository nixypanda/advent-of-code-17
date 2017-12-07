module D4HighEntropyPassphrasesTest
  ( validPassphraseTest
  , anagramTest
  , notAnagramTest
  ) where

import Test.HUnit

import D4HighEntropyPassphrases (isHighEntropy, anagrams, anyAnagrams, notAnagrams)


validPassphraseTest :: [Test]
validPassphraseTest =
  [ TestLabel "Basic" $ TestCase $ assertEqual "aa bb cc dd ee should be True" True $ isHighEntropy (words "aa bb cc dd ee")
  , TestLabel "Basic" $ TestCase $ assertEqual "aa bb cc dd aa should be False" False $ isHighEntropy (words "aa bb cc dd aa")
  , TestLabel "Basic" $ TestCase $ assertEqual "aa bb cc dd aaa should be True" True $ isHighEntropy (words "aa bb cc dd aaa")
  ]


anagramTest :: [Test]
anagramTest =
  [ TestLabel "Anagram" $ TestCase $ assertEqual "abcde fghij are not" False $ anagrams "abcde" "fghij"
  , TestLabel "Anagram" $ TestCase $ assertEqual "abcde ecdab are" True $ anagrams "abcde" "ecdab"
  ]


phrases = unlines
  [ "abcde fghij"
  , "abcde xyz ecdab"
  , "a ab abc abd abf abj"
  , "iiii oiii ooii oooi oooo"
  , "oiii ioii iioi iiio"
  ]

notAnagramTest :: [Test]
notAnagramTest =
  [ TestLabel "Anagram" $ TestCase $ assertEqual "abcde fghij are not" False $ anyAnagrams ["abcde", "fghij"]
  , TestLabel "Anagram" $ TestCase $ assertEqual "abcde fghij are not" True $ anyAnagrams ["abcde", "xyz", "ecdab"]
  , TestLabel "Anagram" $ TestCase $ assertEqual "abcde fghij are not" False $ anyAnagrams (words "a ab abc abd abf abj")
  , TestLabel "Anagram" $ TestCase $ assertEqual "abcde fghij are not" 3 $ notAnagrams phrases
  ]


