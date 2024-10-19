module Lib.HTMLTest (htmlTests) where

import Test.HUnit
import Lib.HTML
import Text.ParserCombinators.Parsec


testOpenTag :: Test
testOpenTag = TestCase (do
    let input = "<body id=\"test\">"
    let result = parse openTag "" input
    let exepected = Right ("body",[("id", "test")])
    assertEqual "parse openTag"  exepected result
    )

testQuotedText :: Test
testQuotedText = TestCase (do
    let input = "\"abc\""
    let result = parse quotedText "" input
    assertEqual "parse quotedText" (Right "abc") result
    )

testQuotedTextWithEscape :: Test
testQuotedTextWithEscape = TestCase (do
    let input = "\"\\\"\""
    let result = parse quotedText "" input
    assertEqual "parse quotedText" (Right "\"") result)


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

testNested2 :: Test
testNested2 = TestCase (do
    let input = "<div><div></div></div>"
    let expected = Right (Element "div" [] [Element "div" [] []])
    let result = parse element "" input
    assertEqual "Parsing nested document" expected result)


htmlTests :: Test
htmlTests = TestList [testOpenTag,testQuotedText,testQuotedTextWithEscape,testElement1,testNested,testNested2]