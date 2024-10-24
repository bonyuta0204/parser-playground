module Lib.Schema (readExpr) where

import Text.ParserCombinators.Parsec hiding (spaces)

-- Parser to consume one or more spaces
spaces :: Parser ()
spaces = skipMany1 space

-- Parser to recognize valid symbols in Lisp
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Data type representing Lisp values
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

-- Parses a Lisp string (quoted with double quotes)
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

-- Parses a Lisp atom (can be a symbol, or boolean literals #t/#f)
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

-- Parses a number
parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

-- Parses a complete Lisp expression (could be atom, string, number, or quoted/list)
parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> ( do
            char '('
            x <- try parseList <|> try parseDottedList
            char ')'
            return x
        )

-- Parses a Lisp list (space-separated expressions enclosed in parentheses)
parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

-- Parses a dotted list (for example: (a b . c))
parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

-- Parses a quoted expression (for example: 'expr becomes (quote expr))
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- Show instance to convert LispVal data types to strings
instance Show LispVal where
  show = showVal

-- Converts Lisp values to their string representation
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

-- Helper function to convert a list of LispVals into a string
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- Function to read and parse Lisp expressions from input strings
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value:\n" ++ show val
