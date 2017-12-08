module D8RegistersTest
  ( regTest
  ) where

import Test.HUnit
import qualified Data.Map as M

import D8Registers
  ( Expr(..)
  , Function(..)
  , Cond(..)
  , Op(..)
  , parseExpr
  , eval
  )

parseTest :: [Test]
parseTest =
  [ TestLabel "Basic" $ TestCase $ assertEqual "" (Right $ Expr "b" Increment 5 (If "a" Gt 1)) $ parseExpr "b inc 5 if a > 1"
  , TestLabel "Basic" $ TestCase $ assertEqual "" (Right $ Expr "a" Increment 1 (If "b" Lt 5)) $ parseExpr "a inc 1 if b < 5"
  , TestLabel "Basic" $ TestCase $ assertEqual "" (Right $ Expr "c" Decrement (-10) (If "a" Gte 1)) $ parseExpr "c dec -10 if a >= 1"
  , TestLabel "Basic" $ TestCase $ assertEqual "" (Right $ Expr "c" Increment (-20) (If "c" Eq 10)) $ parseExpr "c inc -20 if c == 10"
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


evalTest :: [Test]
evalTest =
  [ TestLabel "Basic" $ TestCase $ assertEqual "" [[0,0,0],[0,0,0],[1,0,0],[1,0,10],[1,0,-10]] $ eval m expList
  ]


regTest :: [Test]
regTest = parseTest ++ evalTest
