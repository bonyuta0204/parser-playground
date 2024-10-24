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

calculator :: String -> Either ParseError Int
calculator x = do
  e <- parse expression "(calc)" x
  return $ eval e

data Expression = Val Int | Sum Expression Expression | Sub Expression Expression | Mul Expression Expression | Div Expression Expression deriving (Show, Eq)
