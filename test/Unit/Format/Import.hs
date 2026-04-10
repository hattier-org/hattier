module Unit.Format.Import
  ( tests,
  )
where

import Data.Default (def)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Hattier
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "Import formatting tests"
    [ testCase
        "Correctly print one module import"
        printOneImport,
      testCase
        "Correctly print a list of module imports"
        printImportList,
      testCase
        "Correctly print a module with imports as well as declarations"
        printModuleWithImports
    ]

printOneImport :: IO ()
printOneImport = expectedOutput @=? actualOutput
  where
    testInput =
      T.unlines
        [ "module Nice where",
          "import Nice.Module.Bro"
        ]
    expectedOutput =
      T.init $
        T.unlines
          [ "module Nice where",
            "",
            "import Nice.Module.Bro"
          ]
    source =
      case parseTextToAST testInput defaultParserOpts of
        Right ast' -> ast'
        Left err -> error $ "test fixture failed to parse: " <> show err
    env = Env source def 0
    actualOutput = toStrict $ fst $ execHattier hattier env initialState

printImportList :: IO ()
printImportList = expectedOutput @=? actualOutput
  where
    testInput =
      T.unlines
        [ "module ManyImports where",
          "",
          "import Data.Text qualified as T",
          "import GHC.Hs",
          "import Test.Tasty.HUnit (testCase, (@=?))",
          "import GHC.Types.SrcLoc",
          "import Hattier.Config",
          "import Data.Text.Lazy qualified as T.Lazy",
          "import Hattier.Parser",
          "import Hattier.Types",
          "import Options.Generic (Unwrapped)",
          "import Data.Default (def)",
          "import Hattier.Printer.Declaration.Value.Let",
          "import Test.Tasty (TestTree, testGroup)"
        ]
    expectedOutput =
      T.init $
        T.unlines
          [ "module ManyImports where",
            "",
            "import qualified Data.Text as T",
            "import GHC.Hs",
            "import Test.Tasty.HUnit (testCase, (@=?))",
            "import GHC.Types.SrcLoc",
            "import Hattier.Config",
            "import qualified Data.Text.Lazy as T.Lazy",
            "import Hattier.Parser",
            "import Hattier.Types",
            "import Options.Generic (Unwrapped)",
            "import Data.Default (def)",
            "import Hattier.Printer.Declaration.Value.Let",
            "import Test.Tasty (TestTree, testGroup)"
          ]
    source =
      case parseTextToAST testInput defaultParserOpts of
        Right ast' -> ast'
        Left err -> error $ "test fixture failed to parse: " <> show err
    env = Env source def 0
    actualOutput = toStrict $ fst $ execHattier hattier env initialState

printModuleWithImports :: IO ()
printModuleWithImports = expectedOutput @=? actualOutput
  where
    testInput =
      T.unlines
        [ "module ModuleWithImports where",
          "",
          "import Data.Text qualified as T",
          "import GHC.Hs",
          "import Test.Tasty.HUnit (testCase, (@=?))",
          "import GHC.Types.SrcLoc",
          "",
          "f :: Bool -> Int",
          "f True = 1",
          "f False = 2",
          "func :: (Int, Bool, String, Double) -> (Double, Double)",
          "func _ = (21, 21)"
        ]
    expectedOutput =
      T.init $
        T.unlines
          [ "module ModuleWithImports where",
            "",
            "import qualified Data.Text as T",
            "import GHC.Hs",
            "import Test.Tasty.HUnit (testCase, (@=?))",
            "import GHC.Types.SrcLoc",
            "",
            "f :: Bool -> Int",
            "f True  = 1",
            "f False = 2",
            "",
            "func :: (Int, Bool, String, Double) -> (Double, Double)",
            "func _ = (21, 21)"
          ]
    source =
      case parseTextToAST testInput defaultParserOpts of
        Right ast' -> ast'
        Left err -> error $ "test fixture failed to parse: " <> show err
    env = Env source def 0
    actualOutput = toStrict $ fst $ execHattier hattier env initialState
