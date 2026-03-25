module Main where

import Test.Tasty                      (TestTree, defaultMain, testGroup)
import Unit.Format qualified as Format (tests)
import Unit.Parser qualified as Parser (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Hattier tests"
  [ units
  -- , properties
  -- , spec
  ]

units :: TestTree
units = testGroup "Unit tests"
  [ Format.tests
  , Parser.tests
  ]

-- propertys :: TestTree 
-- propertys = undefined

-- spec :: TestTree
-- spec = undefined
