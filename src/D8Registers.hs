module D8Registers
  ( Expr(..)
  , Cond(..)
  , Op(..)
  , Function(..)
  , parseExpr
  , eval
  , sol8p1
  , sol8p2
  ) where

import Control.Arrow ((&&&))
import Control.Applicative ((<*), (<*>), (<$>))
import Text.ParserCombinators.Parsec
  ( Parser
  , ParseError
  , digit
  , letter
  , string
  , many1
  , many
  , parse
  , char
  , choice
  , try
  , (<|>)
  )

import qualified Data.Map as M

-- Part 1
-- Ok so I have no valid excuse why I am using Parsec here.
-- It's cool I guess ¯\_(ツ)_/¯
-- On a serious note do not use Parsec here (overkill does not do justice here)


-- Expressing our structure

data Function
  = Increment
  | Decrement
  deriving (Show, Eq)

data Op
  = Lt
  | Gt
  | Lte
  | Gte
  | Eq
  | Neq
  deriving (Show, Eq)

data Cond =
  If String Op Int
  deriving (Show, Eq)

data Expr =
  Expr String Function Int Cond
  deriving (Show, Eq)


-- Parsing

ws :: Parser String
ws = many (char ' ')


toFunc :: String -> Function
toFunc "inc" = Increment
toFunc "dec" = Decrement
toFunc _ = error "WTF"


toOp :: String -> Op
toOp "<" = Lt
toOp ">" = Gt
toOp "<=" = Lte
toOp ">=" = Gte
toOp "==" = Eq
toOp "!=" = Neq


int :: Parser Int
int = read
  <$> (ws *> (many1 digit <|> ((:) <$> char '-' <*> many1 digit)) <* ws)


if' :: Parser Cond
if' = If
  <$> (string "if" *> ws *> many1 letter <* ws)
  <*> (toOp <$> choice [ try (string "<=") <|> string "<"
                       , try (string ">=") <|> string ">"
                       , string "=="
                       , string "!="
                       ])
  <*> int


expr :: Parser Expr
expr = Expr
  <$> (many1 letter <* ws)
  <*> (toFunc <$> (string "inc" <|> string "dec") <* ws)
  <*> int
  <*> if'


parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr ""


-- Evaluation

evalCond :: M.Map String Int -> Cond -> Bool
evalCond m (If k op v) =
  let
    v' = m M.! k
  in
    case op of
      Lt  -> v' <  v
      Gt  -> v' >  v
      Lte -> v' <= v
      Gte -> v' >= v
      Eq  -> v' == v
      Neq -> v' /= v


eval :: M.Map String Int -> [Expr] -> [[Int]]
eval m [] = [M.elems m]
eval m (Expr k op v cond:xs) =
  let
    f = if op == Increment then (+ v) else subtract v
  in
    M.elems m : if evalCond m cond then eval (M.adjust f k m) xs else eval m xs


fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "fromRight: Called on Left"


-- being a dick here again (tip read bottom to top)
sol8p1 :: String -> Int
sol8p1 =
    maximum
  . last
  . uncurry eval
  . (M.fromList . (\xs -> [(x, 0) | x <- xs]) . fmap (head . words) &&& fmap (fromRight . parseExpr))
  . lines


-- Part 2

-- well here again
sol8p2 :: String -> Int
sol8p2 =
    maximum
  . fmap maximum
  . uncurry eval
  . (M.fromList . (\xs -> [(x, 0) | x <- xs]) . fmap (head . words) &&& fmap (fromRight . parseExpr))
  . lines
