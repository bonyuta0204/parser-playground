module Main (main) where

import Lib
import Text.ParserCombinators.Parsec

main :: IO ()
main = someFunc

csvFile :: GenParser Char st [[String]]
csvFile = endBy line eol


line :: GenParser Char st [String]
line = sepBy cell (char ',')

cell :: GenParser Char st String 
cell = many (noneOf "\n\r,")

eol :: GenParser Char st String
eol = try (string "\r\n") <|> try (string "\n\r") <|> string "\r" <|> string "\n" <?> "Could not find EOL"

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"