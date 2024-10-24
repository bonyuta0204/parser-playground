module Lib.Calc (expression, calculator, Expression (Val, Sum, Mul, Sub, Div)) where

import Text.ParserCombinators.Parsec

-- データ型定義
data Expression
  = Val Int
  | Sum Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  deriving (Show, Eq)

data MyRational = Q Int Int deriving (Eq, Show)

-- 数値のパーサー
parseValue :: Parser Expression
parseValue = do
  digits <- many1 digit
  return $ Val (read digits :: Int)

-- 括弧内の式をパースする関数
parseSubExpression :: Parser Expression
parseSubExpression = do
  char '('
  expr <- expression
  char ')'
  return expr

-- 掛け算・割り算を含む項のパーサー
parseFactor :: Parser Expression
parseFactor = do
  firstTerm <- parseSubExpression <|> parseValue
  spaces
  parseFactorRest firstTerm
  where
    parseFactorRest leftExpr =
      ( do
          operator <- char '*' <|> char '/'
          spaces
          rightTerm <- parseSubExpression <|> parseValue
          let newExpr = case operator of
                '*' -> Mul leftExpr rightTerm
                '/' -> Div leftExpr rightTerm
          parseFactorRest newExpr
      )
        <|> return leftExpr

-- 足し算・引き算を含む式のパーサー
parseSum :: Parser Expression
parseSum = do
  firstTerm <- parseFactor
  spaces
  parseSumRest firstTerm
  where
    parseSumRest leftExpr =
      ( do
          operator <- char '+' <|> char '-'
          spaces
          rightTerm <- parseFactor
          let newExpr = case operator of
                '+' -> Sum leftExpr rightTerm
                '-' -> Sub leftExpr rightTerm
          parseSumRest newExpr
      )
        <|> return leftExpr

-- 式のパーサー
expression :: Parser Expression
expression = spaces >> parseSum

-- 整数での評価
evaluate :: Expression -> Int
evaluate expr = case expr of
  Val n -> n
  Sum x y -> evaluate x + evaluate y
  Sub x y -> evaluate x - evaluate y
  Mul x y -> evaluate x * evaluate y
  Div x y -> evaluate x `div` evaluate y

-- 分数での評価
evaluateRational :: Expression -> MyRational
evaluateRational expr = case expr of
  Val n -> Q n 1
  Sum x y -> evaluateRational x + evaluateRational y
  Sub x y -> evaluateRational x - evaluateRational y
  Mul x y -> evaluateRational x * evaluateRational y
  Div x y -> evaluateRational x `qdiv` evaluateRational y

-- 整数計算機
calculator :: String -> Either ParseError Int
calculator input = do
  expr <- parse expression "(calc)" input
  return $ evaluate expr

-- 分数計算機
calculatorQ :: String -> Either ParseError MyRational
calculatorQ input = do
  expr <- parse expression "(calc)" input
  return $ evaluateRational expr

-- MyRational 型の演算
instance Num MyRational where
  Q n1 d1 + Q n2 d2 = Q (n1 * d2 + n2 * d1) (d1 * d2)
  Q n1 d1 * Q n2 d2 = Q (n1 * n2) (d1 * d2)
  negate (Q n1 d1) = Q (negate n1) d1

-- 分数の除算
qdiv :: MyRational -> MyRational -> MyRational
qdiv (Q n1 d1) (Q n2 d2) = Q (n1 * d2) (n2 * d1)

-- 最大公約数 (GCD)
myGCD :: Int -> Int -> Int
myGCD x 0 = abs x
myGCD x y = myGCD y (x `mod` y)
