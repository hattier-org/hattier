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
        "formatting an extremely simple module works properly"
        fmtSimpleModule
    ]

fmtSimpleModule :: IO ()
fmtSimpleModule = expectedOutput @=? actualOutput
  where
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
    source =
      case parseTextToAST testInput defaultParserOpts of
        Right ast' -> ast'
        Left err -> error $ "test fixture failed to parse: " <> show err
    env = Env source defaultConfig
    actualOutput = fst (execHattier hattier env initialState)
