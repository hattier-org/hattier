module Unit.Format.Let
  ( tests,
  )
where

import Data.Default (def)
import Data.Text qualified as T
import Data.Text.Lazy qualified as T.Lazy
import GHC.Hs
import GHC.Types.SrcLoc
import Hattier.Config
import Hattier.Parser
import Hattier.Printer.Expression (printLetExpr)
import Hattier.Types
import Options.Generic (Unwrapped)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "Let formatting tests"
    [ testCase "NoAlignment: bindings uniformly indented" noAlignmentTest,
      testCase "PrimaryAlignment: '=' signs aligned" primaryAlignmentTest,
      testCase "NoAlignment: nested let in body" nestedNoAlignmentTest,
      testCase "PrimaryAlignment: nested let, each block aligns" nestedPrimaryAlignmentTest,
      testCase "#32: let binding with a pattern argument" letBindingWithPattern,
      testCase "#32: let bindings with multiple pattern arguments align correctly" letBindingsMultiplePatterns,
      testCase "#32: let binding whose body is a case expression" letBindingWithCase,
      testCase "#32: let binding with guards" letBindingWithGuards
    ]

-- | Run only the let printer against the first let expression found in @src@.
runLetPrinter :: Alignment -> T.Text -> T.Lazy.Text
runLetPrinter style src =
  let m = case parseTextToAST src defaultParserOpts of
        Right ast' -> ast'
        Left err -> error $ "parse error: " <> show err
      (binds, body) = extractLet m
      config = (def :: Config Unwrapped) {letAlignment = style}
      env = Env m config
   in fst (execHattier (printLetExpr binds body) env initialState)

-- | Extract the @HsLocalBinds@ and body from the first let expression in a module.
extractLet :: HattierModule -> (HsLocalBinds GhcPs, LHsExpr GhcPs)
extractLet m =
  case hsmodDecls m of
    (L _ (ValD _ (FunBind _ _ mg)) : _) ->
      case unLoc (mg_alts mg) of
        [L _ (Match _ _ _ (GRHSs _ [L _ (GRHS _ [] (L _ (HsLet _ binds body)))] _))] ->
          (binds, body)
        _ -> error "expected a single let RHS"
    _ -> error "expected a function binding as first declaration"

--- Actual test cases ---

-- | Source with two bindings of different name lengths inside a let.
letSrc :: T.Text
letSrc =
  T.unlines
    [ "module T where",
      "f = let x = 1",
      "        longName = 2",
      "    in x"
    ]

noAlignmentTest :: IO ()
noAlignmentTest = expected @=? runLetPrinter NoAlignment letSrc
  where
    expected = "let x = 1\n      longName = 2\n  in  x"

primaryAlignmentTest :: IO ()
primaryAlignmentTest = expected @=? runLetPrinter PrimaryAlignment letSrc
  where
    --   x        = 1   (x padded to length 8 = length of "longName")
    --   longName = 2
    expected = "let x        = 1\n      longName = 2\n  in  x"

-- | Source with a let nested inside the body of an outer let.
nestedLetSrc :: T.Text
nestedLetSrc =
  T.unlines
    [ "module T where",
      "f = let x = 1",
      "        longName = 2",
      "    in let result = x",
      "       in result"
    ]

nestedNoAlignmentTest :: IO ()
nestedNoAlignmentTest = expected @=? runLetPrinter NoAlignment nestedLetSrc
  where
    expected = "let x = 1\n      longName = 2\n  in  let result = x\n  in  result"

nestedPrimaryAlignmentTest :: IO ()
nestedPrimaryAlignmentTest = expected @=? runLetPrinter PrimaryAlignment nestedLetSrc
  where
    -- outer block: "longName" (8) drives alignment; inner block: "result" (6) drives its own
    expected = "let x        = 1\n      longName = 2\n  in  let result = x\n  in  result"

--- #32 test cases ---

-- | A let binding whose RHS is a function with one pattern argument.
-- Currently hits the @_ -> append "..."@ fallback in 'printBind'.
letBindingWithPattern :: IO ()
letBindingWithPattern = expected @=? runLetPrinter PrimaryAlignment src
  where
    src =
      T.unlines
        [ "module T where",
          "f = let g x = x + 1",
          "    in g 5"
        ]
    -- Single binding, so PrimaryAlignment adds no padding (alignCol = len "g" = 1).
    expected = "let g x = x + 1\n  in  g 5"

-- | Two let bindings each with two pattern arguments; PrimaryAlignment aligns '='.
letBindingsMultiplePatterns :: IO ()
letBindingsMultiplePatterns = expected @=? runLetPrinter PrimaryAlignment src
  where
    src =
      T.unlines
        [ "module T where",
          "f = let add x y = x + y",
          "        sub x y = x - y",
          "    in add 1 2"
        ]
    -- "add" and "sub" are both 3 chars, so alignCol = 3, no extra padding.
    expected = "let add x y = x + y\n      sub x y = x - y\n  in  add 1 2"

-- | A let binding with a pattern argument whose body is a case expression.
-- Requires both pattern support (#32) and correct case-in-let indentation.
letBindingWithCase :: IO ()
letBindingWithCase = expected @=? runLetPrinter PrimaryAlignment src
  where
    src =
      T.unlines
        [ "module T where",
          "f = let g x = case x of",
          "                  0 -> True",
          "                  _ -> False",
          "    in g 5"
        ]
    -- Case branches are indented by indentWidth (2) from the start of the line.
    expected = "let g x = case x of\n  0 -> True\n  _ -> False\n  in  g 5"

-- | A let binding with guards on the RHS.
-- Both guards and their '=' signs should align under PrimaryAlignment.
letBindingWithGuards :: IO ()
letBindingWithGuards = expected @=? runLetPrinter PrimaryAlignment src
  where
    src =
      T.unlines
        [ "module T where",
          "f = let g x",
          "          | x > 0     = 1",
          "          | otherwise = 0",
          "    in g 5"
        ]
    -- Guards are indented by bindInd (indentWidth + 4 = 6 spaces) from the line start.
    -- Note: '=' signs across guards are not aligned (no cross-guard alignment yet).
    expected = "let g x\n      | x > 0 = 1\n      | otherwise = 0\n  in  g 5"
