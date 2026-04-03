module Integration.Let
  ( tests,
  )
where

import Data.Text qualified as T
import Integration.Helpers (runFullFormatter)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "Let integration tests"
    [ testCase "#37: 'in' is indented correctly when let is a function body" letInFunctionBody
    ]

--- #37 regression test ---

-- | The exact source from the bug report: extra spaces in patterns, misplaced 'in'.
letInFunctionBodySrc :: T.Text
letInFunctionBodySrc =
  T.unlines
    [ "module Opener where",
      "",
      "greet :: Int -> String",
      "greet 0 = \"Hello\"",
      "greet 1  = \" INFOAFP\"",
      "greet _   = ",
      "  let hello = greet 0",
      "      course = greet 1",
      "      in hello ++ course"
    ]

-- | The 'in' keyword must be indented (column 2), never at the start of the line.
letInFunctionBody :: IO ()
letInFunctionBody = expected @=? runFullFormatter letInFunctionBodySrc
  where
    expected =
      T.init $
        T.unlines
          [ "module Opener where",
            "",
            "greet :: Int -> String",
            "greet 0 = \"Hello\"",
            "greet 1 = \" INFOAFP\"",
            "greet _ =",
            "  let hello  = greet 0",
            "      course = greet 1",
            "  in hello ++ course"
          ]