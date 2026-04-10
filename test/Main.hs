module Main where

import Integration.Case qualified as IntegrationCase (tests)
import Integration.Let qualified as IntegrationLet (tests)
import Integration.Module qualified as IntegrationModule (tests)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Unit.Format qualified as Format (tests)
import Unit.Parser qualified as Parser (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Hattier tests"
    [ units,
      integrations
      -- , properties
    ]

units :: TestTree
units =
  testGroup
    "Unit tests"
    [ Format.tests,
      Parser.tests
    ]

integrations :: TestTree
integrations =
  testGroup
    "Integration tests"
    [ IntegrationLet.tests,
      IntegrationModule.tests,
      IntegrationCase.tests
    ]

-- propertys :: TestTree
-- propertys = undefined
