module Lib.HTML (openTag, closeTag,element,Node  ) where
import Text.ParserCombinators.Parsec


openTag :: GenParser Char st String
openTag = do
    char '<'
    content <- many (noneOf "<>")
    char '>'
    return content

closeTag :: GenParser Char st String
closeTag = do
    string "</"
    content <- many (noneOf "<>")
    char '>'
    return content

textNode = do
    text <- many1 (noneOf "<>")
    return $ Text text


element :: GenParser Char st Node
element = do
    tag <- openTag
    nodes <- many (try element <|> textNode)
    closeTag
    return (Element tag nodes)


data Node = Element String [Node] | Text String deriving (Show)