
module Lib.Calc (expression, Expression(Val, Sum, Mut))where
import Text.ParserCombinators.Parsec

val :: Parser Expression
val = do
    num <- many1 digit
    return $ Val (read num :: Int)


parseSum :: Parser Expression
parseSum = do
    spaces
    a <- val
    spaces
    char '+'
    spaces
    b <- try parseMul <|> val
    return (Sum a b)


parseMul :: Parser Expression
parseMul = do
    a <- val
    spaces
    char '*'
    spaces
    Mut a <$> val



expression :: Parser Expression
expression = try parseSum <|> try parseMul <|> val





data Expression = Val Int | Sum Expression Expression | Mut Expression Expression deriving(Show,Eq)