module Unit.Format
  ( tests
  ) where

import Test.Tasty (TestTree, testGroup)
import Unit.Format.Function qualified as FormatFunc (tests)
import Unit.Format.Let qualified as FormatLet (tests)

tests :: TestTree
tests = testGroup "Format tests" [FormatFunc.tests, FormatLet.tests]
