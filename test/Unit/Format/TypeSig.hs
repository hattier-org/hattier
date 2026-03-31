-- For now, all this module does is test whether type
-- signatures from the AST don't change semantically.
module Unit.Format.TypeSig
  ( tests,
  )
where

import Data.Default (def)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Hattier
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "Type signature formatting tests"
    [ testCase
        "Print a function with a list argument"
        printListFunc,
      testCase
        "Print a function with a tuple argument and result"
        printTupleFunc,
      testCase
        "Print a function with a tuple argument and result and trim whitespace"
        printTupleFuncWithWhitespace,
      testCase
        "Print a function with a nested list argument and result"
        printNestedListFunc,
      testCase
        "Print a function with a nested tuple list argument and result"
        printNestedTupleListFunc
    ]

runTypeSigTest :: Text -> Text -> IO ()
runTypeSigTest input expected = expected @=? output
  where
    source =
      case parseTextToAST input defaultParserOpts of
        Right ast' -> ast'
        Left err -> error $ "test fixture failed to parse: " <> show err
    env = Env source def
    output = toStrict $ fst $ execHattier hattier env initialState

printListFunc :: IO ()
printListFunc = runTypeSigTest input input
  where
    input = "func :: [Int] -> Bool"

printTupleFunc :: IO ()
printTupleFunc = runTypeSigTest input input
  where
    input = "func :: (Int, Bool, String, Double) -> (Double, Double)"

printTupleFuncWithWhitespace :: IO ()
printTupleFuncWithWhitespace = runTypeSigTest input expected
  where
    input = "func     :: (Int,    Bool, String,    Double)    -> (Double, Double)    "
    expected = "func :: (Int, Bool, String, Double) -> (Double, Double)"

printNestedListFunc :: IO ()
printNestedListFunc = runTypeSigTest input input
  where
    input = "func :: [[[String]]] -> [[[[[[[Int]]]]]]]"

printNestedTupleListFunc :: IO ()
printNestedTupleListFunc = runTypeSigTest input input
  where
    input = "func :: [([Int], String, [[Bool]])] -> [((Int, Int), (Double, Double))]"
