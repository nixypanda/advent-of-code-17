module D8RegistersTest
  ( d8Tests
  ) where

import Test.HUnit
import Text.ParserCombinators.Parsec (ParseError)

import qualified Data.Map as M

import Common
import D8Registers
  ( Expr(..)
  , Function(..)
  , Cond(..)
  , Op(..)
  , parseExpr
  , eval
  )


parseTest :: [TestDefinition String (Either ParseError Expr)]
parseTest =
  [ TD "Basic" "" "b inc 5 if a > 1" (Right $ Expr "b" Increment 5 (If "a" Gt 1))
  , TD "Basic" "" "a inc 1 if b < 5" (Right $ Expr "a" Increment 1 (If "b" Lt 5))
  , TD "Basic" "" "c dec -10 if a >= 1" (Right $ Expr "c" Decrement (-10) (If "a" Gte 1))
  , TD "Basic" "" "c inc -20 if c == 10" (Right $ Expr "c" Increment (-20) (If "c" Eq 10))
  ]


expList :: [Expr]
expList =
  [ Expr "b" Increment 5 (If "a" Gt 1)
  , Expr "a" Increment 1 (If "b" Lt 5)
  , Expr "c" Decrement (-10) (If "a" Gte 1)
  , Expr "c" Increment (-20) (If "c" Eq 10)
  ]


m :: M.Map String Int
m = M.fromList [("a", 0), ("b", 0), ("c", 0)]


evalTest :: [TestDefinition [Expr] [[Int]]]
evalTest =
  [ TD "Basic" "" expList [[0,0,0],[0,0,0],[1,0,0],[1,0,10],[1,0,-10]]
  ]


d8Tests :: [Test]
d8Tests = fmap (apply parseExpr) parseTest ++ fmap (apply $ eval m) evalTest
