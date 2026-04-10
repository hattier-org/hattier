module Hattier.Printer.Expression
  ( printExpr,
    printRHS,
    printLetExpr, -- We only export this because we have isolated tests for let-bindings
  )
where

import Control.Monad.RWS
import Data.Text qualified as T
import GHC.Data.Bag (bagToList)
import GHC.Hs
import GHC.Types.SrcLoc
import Hattier.Config
import Hattier.Printer.Combinators
import Hattier.Printer.Pattern
import Hattier.Printer.Utils
import Hattier.Types

printExpr :: HsExpr GhcPs -> Hattier ()
printExpr (HsCase _ scrut mg) = printCaseExpr scrut mg
printExpr (HsLet _ binds body) = printLetExpr binds body
printExpr expr = fallback expr

-- * case expressions

printCaseExpr :: LHsExpr GhcPs -> MatchGroup GhcPs (LHsExpr GhcPs) -> Hattier ()
printCaseExpr scrut (MG _ (L _ matches)) = do
  anch <- newAnchor
  style <- asks (caseAlignment . cfg)
  let widths = map matchWidth matches
      maxWidth = computeAlignment style widths

  append "case "
  printExpr (unLoc scrut)
  append " of"
  newline

  withAnchor anch $ withSep newline $ map (printAlt maxWidth) matches

-- With NoAlignment, maxWidth should be 0
printAlt :: Int -> LMatch GhcPs (LHsExpr GhcPs) -> Hattier ()
printAlt maxWidth (L _ Match {m_pats = pats, m_grhss = grhss}) = do
  anch <- asks currentAnchor
  indentTo anch

  let patTxt = T.intercalate " " (map pprText pats)
  append $ padTo maxWidth patTxt
  append " -> "

  case grhss of
    GRHSs _ [L _ (GRHS _ [] body)] _ ->
      printExpr (unLoc body)
    GRHSs _ xs _ ->
      withAnchor anch $ printGRHS xs

-- * let expressions

-- | Pretty-print a @let ... in ...@ expression.
-- The style is driven by 'letAlignment' in 'Config'.
printLetExpr :: HsLocalBinds GhcPs -> LHsExpr GhcPs -> Hattier ()
printLetExpr (HsValBinds _ (ValBinds _ binds _sigs)) body = do
  style <- asks (letAlignment . cfg)
  startCol <- gets currentColumn
  let bindList = bagToList binds
      alignCol = computeAlignment style (map nameLen bindList)
      anch = startCol + 4 -- length "let " == 4
  append "let "

  withAnchor anch $ withSep (newline >> indentTo anch) $ map (printBind alignCol) bindList

  newline
  indentTo startCol
  append "in  "
  printExpr (unLoc body)
printLetExpr localBinds body = do
  -- TODO: HsIPBinds and EmptyLocalBinds cases
  fallback localBinds
  newline
  append "in  "
  printExpr (unLoc body)

-- | Print a single binding, padding the name to @alignCol@ columns.
-- When @alignCol = 0@ no padding is added.
printBind :: Int -> LHsBind GhcPs -> Hattier ()
printBind alignCol (L _ (FunBind _ lname mg)) = do
  let name = pprText $ unLoc lname
  append $ padTo alignCol name

  case unLoc (mg_alts mg) of
    [L _ Match {m_pats = pats, m_grhss = grhss}] -> do
      printPats (zip pats (repeat 0))
      printRHS grhss
    -- Empty or multi-clause let bindings are not valid Haskell and
    -- cannot be produced by GHC's parser, so these branches are unreachable.
    -- still, we need to handle them to satisfy the type checker.
    _ -> pure ()
printBind _ bind =
  -- TODO: PatBind and other binding forms
  fallback (unLoc bind)

-- * RHS

-- | Print the RHS of a function clause or let-binding.
-- Handles the "= body" unguarded case (with special layout for let),
-- and the "| guard = body" guarded case.
printRHS :: GRHSs GhcPs (LHsExpr GhcPs) -> Hattier ()
printRHS (GRHSs _ [L _ (GRHS _ [] body)] _) = do
  append " = "
  printExpr (unLoc body)
printRHS (GRHSs _ grhsList _) = do
  anch <- gets currentColumn
  withAnchor anch $ printGRHS grhsList

-- ** guarded RHS

printGRHS :: [LGRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))] -> Hattier ()
printGRHS grhss = do
  anch <- asks currentAnchor
  withSep (newline >> indentTo anch) $ map printGuard grhss

-- | Print a single guarded RHS as @<ind>| <guards> = <body>@.
-- The body printer is passed as a parameter to avoid a circular dependency
-- between this module and the caller's body-printing logic.
printGuard :: LGRHS GhcPs (LHsExpr GhcPs) -> Hattier ()
printGuard (L _ (GRHS _ guards body)) = do
  append " | "
  withSep (append " ") $ map fallback guards
  append " = "
  printExpr (unLoc body)
