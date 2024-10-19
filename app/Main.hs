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
cell = many (noneOf "\n,")

eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"