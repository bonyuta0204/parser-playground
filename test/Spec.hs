import Test.HUnit
import Lib.HTMLTest (htmlTests)


main :: IO Counts
main = runTestTT $ TestList [htmlTests]



