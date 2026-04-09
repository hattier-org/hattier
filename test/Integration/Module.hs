module Integration.Module
  ( tests,
  )
where

import Data.Default (def)
import Data.Text qualified as T
import Hattier.Config
import Integration.Helpers (runFullFormatter, runFullFormatterWith)
import Options.Generic (Unwrapped)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "Module integration tests"
    [ testCase "#37: funcAlignment=PrimaryAlignment (default) - let body formatted correctly" letInFunctionBodyPrimary,
      testCase "#37: funcAlignment=NoAlignment - valid output with let-binding formatting" letInFunctionBodyNoAlign
    ]

--- #37 regression tests ---

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

-- | Default config (funcAlignment=PrimaryAlignment).
letInFunctionBodyPrimary :: IO ()
letInFunctionBodyPrimary = expected @=? runFullFormatter letInFunctionBodySrc
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
            "  in  hello ++ course"
          ]

-- | funcAlignment=NoAlignment: patterns are not padded, but the let body
-- still goes through printLetExpr and produces valid layout.
letInFunctionBodyNoAlign :: IO ()
letInFunctionBodyNoAlign = expected @=? runFullFormatterWith config letInFunctionBodySrc
  where
    config = (def :: Config Unwrapped) {funcAlignment = NoAlignment}
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
            "  in  hello ++ course"
          ]
