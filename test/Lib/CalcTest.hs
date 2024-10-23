module Lib.CalcTest (calcTests) where

import Lib.Calc
import Test.HUnit
import Text.ParserCombinators.Parsec

-- 基本的な加算と乗算のテスト
testExpression1 :: Test
testExpression1 =
  TestCase
    ( do
        let input = "3 + 4 * 5"
        let result = parse expression "" input
        let expected = Right (Sum (Val 3) (Mul (Val 4) (Val 5)))
        assertEqual "expression 3 + 4 * 5" expected result
    )

-- 括弧付きのテスト (期待値を修正)
testExpression2 :: Test
testExpression2 =
  TestCase
    ( do
        let input = "(3 + 4) * 5"
        let result = parse expression "" input
        let expected = Right (Mul (Sum (Val 3) (Val 4)) (Val 5))
        assertEqual "expression (3 + 4) * 5" expected result
    )

-- 単純な数値のテスト
testExpression3 :: Test
testExpression3 =
  TestCase
    ( do
        let input = "42"
        let result = parse expression "" input
        let expected = Right (Val 42)
        assertEqual "expression 42" expected result
    )

-- 加算のみのテスト
testExpression4 :: Test
testExpression4 =
  TestCase
    ( do
        let input = "7 + 3 + 2"
        let result = parse expression "" input
        let expected = Right (Sum (Sum (Val 7) (Val 3)) (Val 2))
        assertEqual "expression 7 + 3 + 2" expected result
    )

-- 異常系のテスト（不正な文字列）
testInvalidExpression :: Test
testInvalidExpression =
  TestCase
    ( do
        let input = "3 + + 4"
        let result = parse expression "" input
        assertBool "invalid expression 3 + + 4" (isLeft result)
    )
    where isLeft (Left _) = True
          isLeft _ = False

-- 複数の括弧がある式
testExpression5 :: Test
testExpression5 =
  TestCase
    ( do
        let input = "(1 + 2) * (3 + 4)"
        let result = parse expression "" input
        let expected = Right (Mul (Sum (Val 1) (Val 2)) (Sum (Val 3) (Val 4)))
        assertEqual "expression (1 + 2) * (3 + 4)" expected result
    )

-- スペースがない式のテスト
testExpressionNoSpaces :: Test
testExpressionNoSpaces =
  TestCase
    ( do
        let input = "3+4*5"
        let result = parse expression "" input
        let expected = Right (Sum (Val 3) (Mul (Val 4) (Val 5)))
        assertEqual "expression 3+4*5" expected result
    )

calcTests = TestList [testExpression1, testExpression2, testExpression3, testExpression4, testInvalidExpression, testExpression5, testExpressionNoSpaces]
