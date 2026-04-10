module Integration.Case
  ( tests,
  )
where

import Data.Text
import Data.Text qualified as T
import Integration.Helpers (runFullFormatter)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "Case expression integration tests"
    [testCase "PrimaryAlignment: correctly align a case statement with guards and nesting" largeCaseWithGuardsAndNesting]

largeCaseWithGuardsAndNesting :: IO ()
largeCaseWithGuardsAndNesting = expected @=? runFullFormatter input

input :: Text
input =
  T.unlines
    [ "f x y = case x of",
      "          2147483647 -> \"A perfect number!\"",
      "          NicePattern bro | bro < y -> case bro * y > 200 of",
      "                                         True -> undefined",
      "                                         wayTooLongNameHere | not $ null wayTooLongNameHere -> \"nice\"",
      "                          | bro == 0 -> let var = map func [1..120] in concatMap numToChar var",
      "          nope -> error \"nope\""
    ]

expected :: Text
expected =
  T.init $
    T.unlines $
      [ "f x y = case x of",
        "          2147483647      -> \"A perfect number!\"",
        "          NicePattern bro | bro < y -> case bro * y > 200 of",
        "                                         True               -> undefined",
        "                                         wayTooLongNameHere | not $ null wayTooLongNameHere -> \"nice\"",
        "                          | bro == 0 -> let var = map func [1 .. 120]",
        "                                        in  concatMap numToChar var",
        "          nope            -> error \"nope\""
      ]
