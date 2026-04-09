module Unit.Format
  ( tests,
  )
where

import Test.Tasty (TestTree, testGroup)
import Unit.Format.Case qualified as FormatCase (tests)
import Unit.Format.Function qualified as FormatFunc (tests)
import Unit.Format.Guard qualified as FormatGuard (tests)
import Unit.Format.Import qualified as FormatImport (tests)
import Unit.Format.Let qualified as FormatLet (tests)
import Unit.Format.TypeSig qualified as FormatTypeSig (tests)

tests :: TestTree
tests =
  testGroup
    "Format tests"
    [ FormatFunc.tests,
      FormatGuard.tests,
      FormatLet.tests,
      FormatCase.tests,
      FormatTypeSig.tests,
      FormatImport.tests
    ]
