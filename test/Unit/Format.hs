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
        testFmtWithBuiltinGHC
    ]

testFmtWithBuiltinGHC :: IO ()
testFmtWithBuiltinGHC = expectedOutput @=? actualOutput
  where
    config = defaultConfig
    testInput =
      T.unlines
        [ "module Example where"
        , ""
        , "f :: Bool -> Int"
        , "f True = 1"
        , "f False = 2"
        ]
    expectedOutput =
      "module Example where\n\nf :: Bool -> Int\nf True = 1\nf False = 2"
    ast =
      case parseTextToAST testInput defaultParserOpts of
        Right a -> a
        Left err -> error $ "test fixture failed to parse: " <> show err
    actualOutput = fst (execHattier (hattier ast) config initialState)
