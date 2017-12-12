module D12DigitalPlumberTest
  ( d12Tests
  ) where

import Test.HUnit

import qualified Data.Map as M
import qualified Data.Set as S

import Common
import D12DigitalPlumber (d12p1, d12p2, toMap, getGroupFor)


iMap :: String
iMap = unlines
  [ "0 <-> 2"
  , "1 <-> 1"
  , "2 <-> 0, 3, 4"
  , "3 <-> 2, 4"
  , "4 <-> 2, 3, 6"
  , "5 <-> 6"
  , "6 <-> 4, 5"
  ]

oMap :: M.Map Int [Int]
oMap = M.fromList
  [ (0, [2])
  , (1, [1])
  , (2, [0, 3, 4])
  , (3, [2, 4])
  , (4, [2, 3, 6])
  , (5, [6])
  , (6, [4, 5])
  ]


toMapTest :: [TestDefinition String (M.Map Int [Int])]
toMapTest =
  [ TD "" "" iMap oMap
  ]

getGroupTest :: [TestDefinition (M.Map Int [Int]) (S.Set Int)]
getGroupTest =
  [ TD "" "" oMap (S.fromList [0, 2, 3, 4, 5, 6])
  ]


d12p1Test :: [TestDefinition String Int]
d12p1Test =
  [ TD "" "" iMap 6
  ]

d12p2Test :: [TestDefinition String Int]
d12p2Test =
  [ TD "" "" iMap 2
  ]

d12Tests :: [Test]
d12Tests = fmap (apply toMap) toMapTest
  ++ fmap (apply (getGroupFor 0)) getGroupTest
  ++ fmap (apply d12p1) d12p1Test
  ++ fmap (apply d12p2) d12p2Test
