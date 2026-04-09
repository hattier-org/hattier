module Main where

import Integration qualified as Integration (tests)
import Test.Tasty (TestTree, defaultIngredients, defaultMainWithIngredients, testGroup)
import Test.Tasty.CoverageReporter (coverageReporter)
import Unit.Format qualified as Format (tests)
import Unit.Parser qualified as Parser (tests)

main :: IO ()
main = defaultMainWithIngredients (coverageReporter : defaultIngredients) tests

tests :: TestTree
tests =
  testGroup
    "Hattier tests"
    [ units,
      Integration.tests
      -- , properties
      -- , spec
    ]

units :: TestTree
units =
  testGroup
    "Unit tests"
    [ Format.tests,
      Parser.tests
    ]

-- propertys :: TestTree
-- propertys = undefined

-- spec :: TestTree
-- spec = undefined
