module Lib.CSV (csvFile) where

import Text.ParserCombinators.Parsec

-- CSV parsing logic
csvFile :: GenParser Char st [[String]]
csvFile = endBy line eol

quotedCell :: GenParser Char st String
quotedCell = do
  char '"'
  content <- many quotedChar
  char '"'
  return content

quotedChar :: GenParser Char st Char
quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

line :: GenParser Char st [String]
line = sepBy cell (char ',')

cell :: GenParser Char st String
cell = quotedCell <|> many (noneOf "\n\r,")

eol :: GenParser Char st String
eol = try (string "\r\n") <|> try (string "\n\r") <|> string "\r" <|> string "\n" <?> "Could not find EOL"
