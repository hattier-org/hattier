module Integration.Let
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
    "Let integration tests"
    -- funcAlignment drives which code path handles the function body.
    -- PrimaryAlignment uses printClause, which calls printExpr on the body.
    -- printExpr does not handle HsLet, so it falls through to pprText, placing
    -- 'in' at column 0 - invalid Haskell. NoAlignment uses pprText for the whole
    -- match clause, which always produces valid output.
    -- letAlignment does not affect this routing, so only funcAlignment matters.
    [ testCase "#37: funcAlignment=PrimaryAlignment (default) - 'in' incorrectly at col 0" letInFunctionBodyPrimary,
      testCase "#37: funcAlignment=NoAlignment - valid output via pprText fallback" letInFunctionBodyNoAlign
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

-- | Default config (funcAlignment=PrimaryAlignment): the failing case.
-- printClause calls printExpr on the HsLet body, which falls through to pprText
-- and produces 'in' at column 0, breaking the GHC parser.
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
            "  in hello ++ course"
          ]

-- | funcAlignment=NoAlignment: the whole match clause goes through pprText,
-- which always produces valid Haskell. This combination already works.
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
            "greet _",
            "  = let",
            "      hello = greet 0",
            "      course = greet 1",
            "    in hello ++ course"
          ]