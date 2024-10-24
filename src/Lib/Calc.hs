module Lib.Calc (expression, Expression (Val, Sum, Mul, Sub, Div)) where

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

data Expression = Val Int | Sum Expression Expression | Sub Expression Expression | Mul Expression Expression | Div Expression Expression deriving (Show, Eq)