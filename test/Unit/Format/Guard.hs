module Unit.Format.Guard
  ( tests,
  )
where

import Data.Default (def)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Hattier
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "Guard formatting tests"
    [ testCase
        "#32: guarded function clauses"
        guardedFunc,
      testCase
        "#32: guarded function clauses with pattern arguments"
        guardedFuncWithPats
    ]

-- | A function with guarded clauses, no pattern arguments.
guardedFunc :: IO ()
guardedFunc = expectedOutput @=? actualOutput
  where
    testInput =
      T.unlines
        [ "module Example where",
          "",
          "classify :: Int -> String",
          "classify x",
          "  | x < 0 = \"negative\"",
          "  | x == 0 = \"zero\"",
          "  | otherwise = \"positive\""
        ]
    expectedOutput =
      T.init $
        T.unlines
          [ "module Example where",
            "",
            "classify :: Int -> String",
            "classify x",
            "  | x < 0 = \"negative\"",
            "  | x == 0 = \"zero\"",
            "  | otherwise = \"positive\""
          ]
    source =
      case parseTextToAST testInput defaultParserOpts of
        Right ast' -> ast'
        Left err -> error $ "test fixture failed to parse: " <> show err
    env = Env source def
    actualOutput = toStrict $ fst $ execHattier hattier env initialState

-- | A function with guarded clauses and two pattern arguments; PrimaryAlignment
-- aligns the patterns across clauses.
guardedFuncWithPats :: IO ()
guardedFuncWithPats = expectedOutput @=? actualOutput
  where
    testInput =
      T.unlines
        [ "module Example where",
          "",
          "safeDiv :: Int -> Int -> Int",
          "safeDiv x y",
          "  | y == 0 = 0",
          "  | otherwise = x `div` y"
        ]
    expectedOutput =
      T.init $
        T.unlines
          [ "module Example where",
            "",
            "safeDiv :: Int -> Int -> Int",
            "safeDiv x y",
            "  | y == 0 = 0",
            "  | otherwise = x `div` y"
          ]
    source =
      case parseTextToAST testInput defaultParserOpts of
        Right ast' -> ast'
        Left err -> error $ "test fixture failed to parse: " <> show err
    env = Env source def
    actualOutput = toStrict $ fst $ execHattier hattier env initialState
