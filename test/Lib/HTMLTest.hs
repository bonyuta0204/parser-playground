module Lib.HTMLTest (htmlTests) where

import Lib.HTML
import Test.HUnit
import Text.ParserCombinators.Parsec

testOpenTag :: Test
testOpenTag =
  TestCase
    ( do
        let input = "<body id=\"test\">"
        let result = parse openTag "" input
        let exepected = Right ("body", [("id", "test")])
        assertEqual "parse openTag" exepected result
    )

testOpenTagMultiLine :: Test
testOpenTagMultiLine =
  TestCase
    ( do
        let input = "<body\n id=\"test\"\n>"
        let result = parse openTag "" input
        let exepected = Right ("body", [("id", "test")])
        assertEqual "parse openTag" exepected result
    )

testOpenTagMultiLine2 :: Test
testOpenTagMultiLine2 =
  TestCase
    ( do
        let input = "<script\n    type=\"text/javascript\"\n    async=\"\"\n    src=\"//test.js\"\n  ></script>"
        let result = parse openTag "" input
        let exepected = Right ("script", [("type", "text/javascript"), ("async", ""), ("src", "//test.js")])
        assertEqual "parse openTag" exepected result
    )

testVoidElement :: Test
testVoidElement =
  TestCase
    ( do
        let input = "<meta charset=\"utf-8\" />"
        let result = parse voidElement "" input
        let exepected = Right (Element "meta" [("charset","utf-8")] [])
        assertEqual "parse voidTag" exepected result
    )

testQuotedText :: Test
testQuotedText =
  TestCase
    ( do
        let input = "\"abc\""
        let result = parse quotedText "" input
        assertEqual "parse quotedText" (Right "abc") result
    )

testComment :: Test
testComment =
  TestCase
    ( do
        let input = "<!--comment-->"
        let result = parse comment "" input
        assertEqual "parse quotedText" (Right "comment") result
    )

testQuotedTextWithEscape :: Test
testQuotedTextWithEscape =
  TestCase
    ( do
        let input = "\"\\\"\""
        let result = parse quotedText "" input
        assertEqual "parse quotedText" (Right "\"") result
    )

testElement1 :: Test
testElement1 =
  TestCase
    ( do
        let input = "<div>Hello, World!</div>"
        let expected = Right (Element "div" [] [Text "Hello, World!"])
        let result = parse element "" input
        assertEqual "Parsing <div>Hello, World!</div>" expected result
    )

testNested :: Test
testNested =
  TestCase
    ( do
        let input = "<html><body></body></html>"
        let expected = Right (Element "html" [] [Element "body" [] []])
        let result = parse element "" input
        assertEqual "Parsing nested document" expected result
    )

testNested2 :: Test
testNested2 =
  TestCase
    ( do
        let input = "<div><div></div></div>"
        let expected = Right (Element "div" [] [Element "div" [] []])
        let result = parse element "" input
        assertEqual "Parsing nested document" expected result
    )

testSingleTag :: Test
testSingleTag =
  TestCase
    ( do
        let input = "<div>a<br>b</div>"
        let expected = Right (Element "div" [] [Text "a", Element "br" [] [], Text "b"])
        let result = parse element "" input
        assertEqual "Parsing element with single tag" expected result
    )

testWithComment :: Test
testWithComment =
  TestCase
    ( do
        let input = "<div><!--comment-->a</div>"
        let expected = Right (Element "div" [] [Text "", Text "a"])
        let result = parse element "" input
        assertEqual "Parsing element with comment" expected result
    )

htmlTests :: Test
htmlTests = TestList [testOpenTag, testOpenTagMultiLine, testOpenTagMultiLine2, testVoidElement, testQuotedText, testQuotedTextWithEscape, testElement1, testNested, testComment, testWithComment, testSingleTag, testNested2]