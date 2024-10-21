module Lib.CalcTest (calcTests) where

import  Lib.Calc
import Test.HUnit
import Text.ParserCombinators.Parsec

testExpression1 :: Test
testExpression1 =
  TestCase
    ( do
        let input = "3 + 4 * 5"
        let result = parse expression "" input
        let exepected = Right (Sum (Val 3) (Mut (Val 4)  (Val 5)) )
        assertEqual "expression" exepected result
    )

calcTests = TestList [testExpression1]