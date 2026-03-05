module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Parser (parseFileToAST, defaultParserOpts)
import System.FilePath ((</>))
import GHC.Types.SrcLoc (unLoc)
import GHC.Hs (hsmodDecls, hsmodName)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "parse example file successfully" testParseExample,
    testCase "top-level declarations equals 2" testTopLevelDeclsEqualsTwo,
    testCase "module name equals Example" testModuleNameEqualsExample
  ]

testParseExample :: Assertion
testParseExample = do
  result <- parseFileToAST ("test" </> "Example.hs") defaultParserOpts
  case result of
    Left err -> assertFailure $ "Failed to parse: " ++ show err
    Right _ -> return ()

testTopLevelDeclsEqualsTwo :: Assertion
testTopLevelDeclsEqualsTwo = do
  result <- parseFileToAST ("test" </> "Example.hs") defaultParserOpts
  case result of
    Left err -> assertFailure $ "Failed to parse: " ++ show err
    Right ast -> do
      let declCount = length (hsmodDecls (unLoc ast))
      assertEqual "Top-level declaration count" 2 declCount

testModuleNameEqualsExample :: Assertion
testModuleNameEqualsExample = do
  result <- parseFileToAST ("test" </> "Example.hs") defaultParserOpts
  case result of
    Left err -> assertFailure $ "Failed to parse: " ++ show err
    Right ast -> do
      let moduleNameStr = case hsmodName (unLoc ast) of
            Just modName -> showSDocUnsafe (ppr modName)
            Nothing -> ""
      assertEqual "Module name" "Example" moduleNameStr
