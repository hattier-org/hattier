module Unit.Format
  ( tests
  ) where

import Data.Text as T hiding (show)
import Hattier
import Hattier.Parser
import Hattier.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?), testCase)

tests :: TestTree
tests =
  testGroup
    "Format tests"
    [ testCase
        "formatting with the GHC pretty printer works properly"
        fmtWithBuiltinGHC
    ]

fmtWithBuiltinGHC :: IO ()
fmtWithBuiltinGHC =
  expectedOutput @=? fst (execHattier hattier config (testState testInput))
  where
    config = Config 2 80
    testInput =
      T.unlines
        [ "module Example where"
        , ""
        , "f :: Bool -> Int"
        , "f True = 1"
        , "f False = 2"
        ]
    expectedOutput =
      "module Example where\nf :: Bool -> Int\nf True = 1\nf False = 2"

testState :: Text -> FormatterState
testState input = FormatterState {parsedModule = ast, outputText = mempty}
  where
    ast =
      case parseTextToAST input defaultParserOpts of
        Right a -> a
        Left err -> error $ "test fixture failed to parse: " <> show err
