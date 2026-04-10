module Unit.Format.Function
  ( tests,
  )
where

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
    "Function formatting tests"
    [ testCase
        "PrimaryAlignment: formatting an extremely simple module works properly"
        alignSimpleModule,
      testCase
        "NoAlignment: formatting an extremely simple module works properly"
        dontAlignSimpleModule,
      testCase
        "PrimaryAlignment: align a function with a list pattern correctly"
        alignListFunc,
      testCase
        "PrimaryAlignment: align a function with a tuple pattern correctly"
        alignTupleFunc
    ]

testInput1 :: Text
testInput1 =
  T.unlines
    [ "module Example where",
      "",
      "f :: Bool -> Int",
      "f True = 1",
      "f False = 2"
    ]

alignSimpleModule :: IO ()
alignSimpleModule = expectedOutput @=? actualOutput
  where
    expectedOutput =
      T.init $ -- remove final newline character (we don't want it)
        T.unlines
          [ "module Example where",
            "",
            "f :: Bool -> Int",
            "f True  = 1",
            "f False = 2"
          ]
    -- "module Example where\n\nf :: Bool -> Int\nf True  = 1\nf False = 2"
    source =
      case parseTextToAST testInput1 defaultParserOpts of
        Right ast' -> ast'
        Left err -> error $ "test fixture failed to parse: " <> show err
    env = Env source def 1
    actualOutput = toStrict $ fst $ execHattier hattier env initialState

dontAlignSimpleModule :: IO ()
dontAlignSimpleModule = expectedOutput @=? actualOutput
  where
    expectedOutput =
      T.init $
        T.unlines
          [ "module Example where",
            "",
            "f :: Bool -> Int",
            "f True = 1",
            "f False = 2"
          ]
    source =
      case parseTextToAST testInput1 defaultParserOpts of
        Right ast' -> ast'
        Left err -> error $ "test fixture failed to parse: " <> show err
    config = (def :: Config Unwrapped) {funcAlignment = NoAlignment}
    env = Env source config 1
    actualOutput = toStrict $ fst $ execHattier hattier env initialState

alignListFunc :: IO ()
alignListFunc = expectedOutput @=? actualOutput
  where
    testInput =
      T.unlines
        [ "func :: [Int] -> Bool",
          "func (1 : 2 : []) = True",
          "func (x:xs) = func xs",
          "func [] = False",
          "func _ = False"
        ]
    expectedOutput =
      T.init $
        T.unlines
          [ "func :: [Int] -> Bool",
            "func (1 : 2 : []) = True",
            "func (x : xs)     = func xs",
            "func []           = False",
            "func _            = False"
          ]
    source =
      case parseTextToAST testInput defaultParserOpts of
        Right ast' -> ast'
        Left err -> error $ "test fixture failed to parse: " <> show err
    env = Env source def 1
    actualOutput = toStrict $ fst $ execHattier hattier env initialState

alignTupleFunc :: IO ()
alignTupleFunc = expectedOutput @=? actualOutput
  where
    testInput =
      T.unlines
        [ "func :: (Int, Int) -> Bool",
          "func (23, 2) = True",
          "func (0, longArgName) = True",
          "func _ = False"
        ]
    expectedOutput =
      T.init $
        T.unlines
          -- Note that 'longArgName' should ideally be spaced one
          -- column to the right. This should be fixed in
          -- 'Hattier.Printer.Declaration.Value.Function.printPatsWithPadding'
          [ "func :: (Int, Int) -> Bool",
            "func (23, 2)          = True",
            "func (0, longArgName) = True",
            "func _                = False"
          ]
    source =
      case parseTextToAST testInput defaultParserOpts of
        Right ast' -> ast'
        Left err -> error $ "test fixture failed to parse: " <> show err
    env = Env source def 1
    actualOutput = toStrict $ fst $ execHattier hattier env initialState
