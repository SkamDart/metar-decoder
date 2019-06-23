import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain unitTests

unitTests = testGroup "Unit Tests"
    [mkTestSomething
    ]

mkTestSomething =
    testCase "foo" $ assertEqual [] "foo" "foo"
