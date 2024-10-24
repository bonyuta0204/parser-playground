import Lib.CalcTest (calcTests)
import Lib.HTMLTest (htmlTests)
import Test.HUnit

main :: IO Counts
main = runTestTT $ TestList [htmlTests, calcTests]
