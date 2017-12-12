module D11HexEdTest
  ( d11Tests
  ) where

import Test.HUnit

import Common
import D11HexEd (d11p1)


d11p1Tests :: [TestDefinition String Int]
d11p1Tests =
  [ TD "" "" "ne,ne,ne" 3
  , TD "" "" "ne,ne,sw,sw" 0
  , TD "" "" "ne,ne,s,s" 2
  , TD "" "" "se,sw,se,sw,sw" 3
  ]

d11Tests :: [Test]
d11Tests = fmap (apply d11p1) d11p1Tests


