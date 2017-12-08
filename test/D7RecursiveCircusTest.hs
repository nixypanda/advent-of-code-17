module D7RecursiveCircusTest
  ( recTests
  ) where

import Test.HUnit

import D7RecursiveCircus (Rose(..), getHead, readMap, stringToRose, getInvalidNode, sol7p2)


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



headTests :: [Test]
headTests =
  [ TestLabel "Basic" $ TestCase $ assertEqual "" "tknk" $ getHead diskMap
  ]

readTest :: [Test]
readTest =
  [ TestLabel "Basic" $ TestCase $ assertEqual "" diskMap $ readMap strMap
  , TestLabel "Basic" $ TestCase $ assertEqual "" tree $ stringToRose strMap
  , TestLabel "Basic" $ TestCase $ assertEqual "" ([251, 243, 243], [68, 45, 72]) $ sol7p2 strMap
  , TestLabel "Basic" $ TestCase $ assertEqual "" "ugml" $ getInvalidNode tree
  ]

recTests :: [Test]
recTests = headTests ++ readTest

