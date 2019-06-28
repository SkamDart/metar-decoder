import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO ()
main = defaultMain unitTests

unitTests = testGroup "Unit Tests"
    [mkTestSomething
    , mkTestMapTriple
    ]

mkTestSomething =
    testCase "foo" $ assertEqual [] "foo" "foo"

mkTestMapTriple =
    testCase "MapTriple" $ assertEqual [] (mapTriple (+1) (1, 2, 3)) (2, 3, 4)
