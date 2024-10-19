module Lib.HTMLTest (htmlTests) where

import Test.HUnit
import Lib.HTML
import Text.ParserCombinators.Parsec



testElement1 :: Test
testElement1 = TestCase (do
    let input = "<div>Hello, World!</div>"
    let expected = Right (Element "div" [] [Text "Hello, World!"])
    let result = parse element "" input
    assertEqual "Parsing <div>Hello, World!</div>" expected result)

htmlTests :: Test
htmlTests = TestList [testElement1]