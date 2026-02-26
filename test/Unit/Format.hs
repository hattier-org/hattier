module Unit.Format (tests) where

import Test.Tasty                 (TestTree, testGroup)
import Test.Tasty.HUnit           (testCase, (@?=))
import Types                      (Config(..), execHattier)
import Format qualified as Format (fmtExample)

tests :: TestTree
tests = testGroup "Format tests"
  [ testCase "example formatter works properly" fmtExample
  ]

fmtExample :: IO ()
fmtExample = res @?= fst (execHattier Format.fmtExample config sourceFiles)
  where
    config      = Config 2 80
    sourceFiles = ["foo\nbar\n"    , "baz\nhaz\n"    ]
    res         = ["  foo\n  bar\n", "  baz\n  haz\n"]
