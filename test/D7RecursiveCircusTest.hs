module D7RecursiveCircusTest
  ( d7Tests
  ) where

import Test.HUnit

import Common
import D7RecursiveCircus (Rose(..), getHead, readMap, stringToRose, getInvalidNode, d7p2)


diskMap :: [(String, Int, [String])]
diskMap =
  [ ("pbga", 66, [])
  , ("xhth", 57, [])
  , ("ebii", 61, [])
  , ("havc", 66, [])
  , ("ktlj", 57, [])
  , ("fwft", 72, ["ktlj", "cntj", "xhth"])
  , ("qoyq", 66, [])
  , ("padx", 45, ["pbga", "havc", "qoyq"])
  , ("tknk", 41, ["ugml", "padx", "fwft"])
  , ("jptl", 61, [])
  , ("ugml", 68, ["gyxo", "ebii", "jptl"])
  , ("gyxo", 61, [])
  , ("cntj", 57, [])
  ]

strMap :: String
strMap = unlines
  [ "pbga (66)"
  , "xhth (57)"
  , "ebii (61)"
  , "havc (66)"
  , "ktlj (57)"
  , "fwft (72) -> ktlj, cntj, xhth"
  , "qoyq (66)"
  , "padx (45) -> pbga, havc, qoyq"
  , "tknk (41) -> ugml, padx, fwft"
  , "jptl (61)"
  , "ugml (68) -> gyxo, ebii, jptl"
  , "gyxo (61)"
  , "cntj (57)"
  ]

tree :: Rose String Int
tree =
  Rose "tknk" 41
    [ Rose "ugml" 68
      [ Rose "gyxo" 61 []
      , Rose "ebii" 61 []
      , Rose "jptl" 61 []
      ]
    , Rose "padx" 45
      [ Rose "pbga" 66 []
      , Rose "havc" 66 []
      , Rose "qoyq" 66 []
      ]
    , Rose "fwft" 72
      [ Rose "ktlj" 57 []
      , Rose "cntj" 57 []
      , Rose "xhth" 57 []
      ]
    ]


headTests :: [TestDefinition [(String, Int, [String])] String]
headTests =
  [ TD "" "" diskMap "tknk"
  ]


toRoseTests :: [TestDefinition String (Rose String Int)]
toRoseTests =
  [ TD "" "" strMap tree
  ]


d7p2Tests :: [TestDefinition String ([Int], [Int])]
d7p2Tests =
  [ TD "" "" strMap ([251, 243, 243], [68, 45, 72])
  ]


invalidNodeTests :: [TestDefinition (Rose String Int) String]
invalidNodeTests =
  [ TD "" "" tree "ugml"
  ]


d7Tests :: [Test]
d7Tests = fmap (apply getHead) headTests
  ++ fmap (apply stringToRose) toRoseTests
  ++ fmap (apply d7p2) d7p2Tests
  ++ fmap (apply getInvalidNode) invalidNodeTests

