module Lib.HTML (openTag, closeTag,element,html,Node(Element, Text)  ) where
import Text.ParserCombinators.Parsec


openTag :: GenParser Char st (String, [Attribute])
openTag = do
    char '<'
    content <- many (noneOf "<>")
    char '>'
    return (content,[])

closeTag :: GenParser Char st String
closeTag = do
    string "</"
    content <- many (noneOf "<>")
    char '>'
    return content

textNode :: GenParser Char st Node
textNode = do
    text <- many1 (noneOf "<>")
    return $ Text text

element :: GenParser Char st Node
element = do
    (tag, attributes) <- openTag
    nodes <- many (try element <|> textNode)
    closingTag <- closeTag
    if tag == closingTag
        then return (Element tag attributes nodes)
        else fail $ "Expected closing tag </" ++ tag ++ ">, but got </" ++ closingTag ++ ">"

html :: GenParser Char st [Node]
html = many element

type Attribute = (String,String)
data Node = Element String [Attribute] [Node] | Text String deriving (Show, Eq)