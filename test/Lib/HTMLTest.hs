module Lib.HTMLTest (htmlTests) where

import Test.HUnit
import Lib.HTML
import Text.ParserCombinators.Parsec


testOpenTag = TestCase (do
    let input = "<body>"
    let result = parse (openTag >> eof) "" input
    assertEqual "parse openTag" (Right ()) result
    )


testElement1 :: Test
testElement1 = TestCase (do
    let input = "<div>Hello, World!</div>"
    let expected = Right (Element "div" [] [Text "Hello, World!"])
    let result = parse element "" input
    assertEqual "Parsing <div>Hello, World!</div>" expected result)

testNested :: Test
testNested = TestCase (do
    let input = "<html><body></body></html>"
    let expected = Right (Element "html" [] [Element "body" [] []])
    let result = parse element "" input
    assertEqual "Parsing nested document" expected result)

htmlTests :: Test
htmlTests = TestList [testOpenTag,testElement1,testNested]