module Unit.Format.Case (tests) where

import Data.Default (def)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Hattier
import Options.Generic (Unwrapped)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "Case formatting tests"
    [ testCase "PrimaryAlignment: simple case expression aligns arrows" alignSimpleCase,
      testCase "NoAlignment: simple case expression does not align arrows" noAlignSimpleCase
    ]

alignSimpleCase :: IO ()
alignSimpleCase = expectedOutput @=? actualOutput
  where
    actualOutput = runFormatter def input
    expectedOutput =
      T.init $
        T.unlines
          [ "module Example where",
            "",
            "f x = case x of",
            "        A   -> 1",
            "        BBB -> 2",
            "        C   -> 3"
          ]
    input =
      T.unlines
        [ "module Example where",
          "",
          "f x = case x of",
          "  A -> 1",
          "  BBB -> 2",
          "  C -> 3"
        ]

noAlignSimpleCase :: IO ()
noAlignSimpleCase = expectedOutput @=? actualOutput
  where
    actualOutput = runFormatter config input
    config = (def :: Config Unwrapped) {caseAlignment = NoAlignment}
    expectedOutput =
      T.init $
        T.unlines
          [ "module Example where",
            "",
            "f x = case x of",
            "        A -> 1",
            "        BBB -> 2",
            "        C -> 3"
          ]
    input =
      T.unlines
        [ "module Example where",
          "",
          "f x = case x of",
          "  A -> 1",
          "  BBB -> 2",
          "  C -> 3"
        ]

runFormatter :: Config Unwrapped -> Text -> Text
runFormatter config input =
  let source =
        case parseTextToAST input defaultParserOpts of
          Right ast' -> ast'
          Left err -> error $ "test fixture failed to parse: " <> show err
      env = Env source config 0
   in toStrict $ fst $ execHattier hattier env initialState
