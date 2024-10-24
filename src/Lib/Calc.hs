module Lib.Calc (expression, calculator, Expression (Val, Sum, Mul, Sub, Div)) where

import Text.ParserCombinators.Parsec

val :: Parser Expression
val = do
  num <- many1 digit
  return $ Val (read num :: Int)

termSum :: Parser Expression
termSum = do
  a <- termFactor
  spaces
  rest a
  where
    rest left =
      ( do
          s <- char '+' <|> char '-'
          spaces
          b <- termFactor
          spaces
          rest $ case s of
            '+' -> Sum left b
            '-' -> Sub left b
            _ -> b
      )
        <|> return left

termFactor :: Parser Expression
termFactor = do
  a <- subExpression <|> val
  spaces
  rest a
  where
    rest left =
      ( do
          s <- char '*' <|> char '/'
          spaces
          b <- subExpression <|> val
          rest $ case s of
            '*' -> Mul left b
            '/' -> Div left b
            _ -> left
      )
        <|> return left

subExpression :: Parser Expression
subExpression = do
  char '('
  e <- expression
  char ')'
  return e

expression :: Parser Expression
expression = spaces >> termSum

eval :: Expression -> Int
eval e = case e of
  Val x -> x
  Sum x y -> eval x + eval y
  Sub x y -> eval x - eval y
  Mul x y -> eval x * eval y
  Div x y -> eval x `div` eval y

evalQ :: Expression -> MyRational
evalQ e = case e of
  Val x -> Q x 1
  Sum x y -> evalQ x + evalQ y
  Sub x y -> evalQ x - evalQ y
  Mul x y -> evalQ x * evalQ y
  Div x y -> evalQ x `qdiv` evalQ y

calculator :: String -> Either ParseError Int
calculator x = do
  e <- parse expression "(calc)" x
  return $ eval e

calculatorQ :: String -> Either ParseError MyRational
calculatorQ x = do
  e <- parse expression "(calc)" x
  return $ evalQ e

data Expression = Val Int | Sum Expression Expression | Sub Expression Expression | Mul Expression Expression | Div Expression Expression deriving (Show, Eq)

data MyRational = Q Int Int deriving (Eq, Show)

instance Num MyRational where
  Q x1 y1 + Q x2 y2 = Q (x1 * s1 + x2 * s2) (g * s1 * s2)
    where
      g = myGCD y1 y2
      s1 = y2 `div` g
      s2 = y1 `div` g

qdiv (Q x1 y1) (Q x2 y2) = Q (x1 * y2) (x2 * y1)

-- x = 12
-- y = 18
myGCD :: Int -> Int -> Int
myGCD x y = case q of
  0 -> r
  _ -> myGCD q r
  where
    (q, r) =
      if x > y
        then (x `mod` y, y)
        else (y `mod` x, x)