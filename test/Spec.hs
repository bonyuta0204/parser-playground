import Test.HUnit
import Lib.HTMLTest (htmlTests)
import Lib.CalcTest (calcTests)


main :: IO Counts
main = runTestTT $ TestList [htmlTests, calcTests]



