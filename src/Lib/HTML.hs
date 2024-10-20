module Lib.HTML (openTag, closeTag, element, voidElement,quotedText, html, comment, Node (Element, Text)) where

import Debug.Trace (trace)
import Text.Parsec (parserTrace)
import Text.ParserCombinators.Parsec

openTag :: GenParser Char st (String, [Attribute])
openTag = do
  char '<'
  tagName <- many1 (letter <|> digit)
  spaces
  attributes <- try (many attribute) <|> return []
  optional spaces
  char '>'
  return (tagName, attributes)


attribute :: GenParser Char st Attribute
attribute = do
  spaces
  key <- many1 (noneOf "=<> /")
  char '='
  value <- quotedText
  spaces
  return (key, value)

quotedText :: GenParser Char st String
quotedText = do
  char '"'
  text <- many (escapeChar <|> noneOf "\"\\")
  char '"'
  return text

escapeChar :: GenParser Char st Char
escapeChar = do
  try (string "\\\"" >> return '"')

closeTag :: GenParser Char st String
closeTag = do
  string "</"
  content <- many (letter <|> digit)
  spaces
  char '>'
  return content

comment :: GenParser Char st String
comment = do
  string "<!--"
  manyTill anyChar (try (string "-->"))

textNode :: GenParser Char st Node
textNode = do
  text <- many1 (noneOf "<")
  return $ Text text

voidTags = ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]

voidElement :: GenParser Char st Node
voidElement = do
  char '<'
  tagName <- choice $ map (try . string) voidTags
  spaces
  attributes <- try (many attribute) <|> return []
  skipMany (space <|> char '/')
  char '>'
  return $ Element tagName attributes []

parseNode :: GenParser Char st Node
parseNode = do
  choice [try voidElement, try element, try (comment >> return (Text "")), textNode]

element :: GenParser Char st Node
element = do
  (tag, attributes) <- openTag
  _ <- trace ("\n ===tag: " ++ tag ++ "===\n") (return ())
  nodes <- many parseNode
  _ <- trace ("\n ===Node (" ++ tag ++ ") Parsed: " ++ show nodes ++ "===\n") (return ())
  closingTag <- closeTag
  if tag == closingTag
    then return (Element tag attributes nodes)
    else fail $ "Expected closing tag </" ++ tag ++ ">, but got </" ++ closingTag ++ ">"

html :: GenParser Char st [Node]
html = many element

type Attribute = (String, String)

data Node = Element String [Attribute] [Node] | Text String deriving (Show, Eq)