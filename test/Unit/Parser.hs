module Unit.Parser
  ( tests
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Hs (hsmodDecls, hsmodName)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import Hattier.Parser (defaultParserOpts, parseTextToAST)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Parser tests"
    [ testCase "parse example file successfully" testParseExample
    , testCase "top-level declarations equals 2" testTopLevelDeclsEqualsTwo
    , testCase "module name equals Example" testModuleNameEqualsExample
    ]

testParseExample :: Assertion
testParseExample = do
  let result = parseTextToAST exampleInput defaultParserOpts
  case result of
    Left err -> assertFailure $ "Failed to parse: " ++ show err
    Right _ -> return ()

testTopLevelDeclsEqualsTwo :: Assertion
testTopLevelDeclsEqualsTwo = do
  let result = parseTextToAST exampleInput defaultParserOpts
  case result of
    Left err -> assertFailure $ "Failed to parse: " ++ show err
    Right ast -> do
      let declCount = length (hsmodDecls ast)
      assertEqual "Top-level declaration count" 2 declCount

testModuleNameEqualsExample :: Assertion
testModuleNameEqualsExample = do
  let result = parseTextToAST exampleInput defaultParserOpts
  case result of
    Left err -> assertFailure $ "Failed to parse: " ++ show err
    Right ast -> do
      let moduleNameStr =
            case hsmodName ast of
              Just modName -> showSDocUnsafe (ppr modName)
              Nothing -> ""
      assertEqual "Module name" "Example" moduleNameStr

exampleInput :: Text
exampleInput =
  T.unlines
    ["module Example where", "", "example :: Int -> Int", "example x = x + 1"]
