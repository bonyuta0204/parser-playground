
module Lib.Calc (expression, Expression(Val, Sum, Mul))where
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
    rest left = (do
        _ <- char '+'
        spaces
        b <- termFactor
        spaces
        rest $ Sum left b
      ) <|> return left


termFactor :: Parser Expression
termFactor = do
    a <- val
    spaces
    rest a
  where
    rest left = (do
      _ <- char '*'
      spaces
      b <- val
      rest $ Mul left b
      ) <|> return left




expression :: Parser Expression
expression = spaces >> termSum





data Expression = Val Int | Sum Expression Expression | Mul Expression Expression deriving(Show,Eq)