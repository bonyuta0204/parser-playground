module Lib.HTML (openTag, closeTag, element, quotedText, html, comment, Node (Element, Text)) where

import Text.ParserCombinators.Parsec

openTag :: GenParser Char st (String, [Attribute])
openTag = do
  char '<'
  tagName <- many letter
  attributes <- try (char ' ' >> sepBy attribute (char ' ')) <|> return []
  char '>'
  return (tagName, attributes)

attribute :: GenParser Char st Attribute
attribute = do
  key <- many letter
  char '='
  value <- quotedText
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
  content <- many (noneOf "<>")
  char '>'
  return content

comment :: GenParser Char st String
comment = between (string "<!--") (string "-->") (many (noneOf "<!-"))

textNode :: GenParser Char st Node
textNode = do
  text <- many1 (noneOf "<>")
  return $ Text text

singleElement :: GenParser Char st Node
singleElement = do
  (tagName, at) <- openTag
  return $ Element tagName at []

element :: GenParser Char st Node
element = do
  (tag, attributes) <- openTag
  nodes <- many (try element <|> try singleElement <|> try (comment >> return (Text "")) <|> textNode)
  closingTag <- closeTag
  if tag == closingTag
    then return (Element tag attributes nodes)
    else fail $ "Expected closing tag </" ++ tag ++ ">, but got </" ++ closingTag ++ ">"

html :: GenParser Char st [Node]
html = many element

type Attribute = (String, String)

data Node = Element String [Attribute] [Node] | Text String deriving (Show, Eq)