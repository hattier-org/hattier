module Integration
  ( tests,
  )
where

import Integration.Case qualified as IntegrationCase (tests)
import Integration.Let qualified as IntegrationLet (tests)
import Integration.Module qualified as IntegrationModule (tests)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Integration tests"
    [ IntegrationLet.tests,
      IntegrationModule.tests,
      IntegrationCase.tests
    ]
