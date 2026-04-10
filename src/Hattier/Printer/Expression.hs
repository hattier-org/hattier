-- | Printer for Haskell expressions and right-hand sides.
--
-- Handles @case@ and @let@ expressions directly, and the guarded\/unguarded
-- RHS forms that appear in both function clauses and case alternatives.
module Hattier.Printer.Expression
  ( printExpr,
    printRHS,
    printLetExpr, -- We only export this because we have isolated tests for let-bindings
    RHSprefix (..),
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

-- | The separator token that precedes a right-hand side body:
-- @=@ for function\/let bindings ('EqualsSign'), @->@ for case alternatives ('ArrowSign').
data RHSprefix = EqualsSign | ArrowSign

-- | Print a Haskell expression. Handles @case@ and @let@ directly;
-- falls back to 'ppr' for all other expression forms.
printExpr :: HsExpr GhcPs -> Hattier
printExpr (HsCase _ scrut mg) = printCaseExpr scrut mg
printExpr (HsLet _ binds body) = printLetExpr binds body
printExpr expr = fallback expr

-- * case expressions

printCaseExpr :: LHsExpr GhcPs -> MatchGroup GhcPs (LHsExpr GhcPs) -> Hattier
printCaseExpr scrut (MG _ (L _ matches)) = do
  style <- asks (caseAlignment . cfg)
  startCol <- gets currentColumn
  indW <- asks (fromIntegral . indentWidth . cfg)
  let widths = map matchWidth matches
      maxWidth = computeAlignment style widths
      anch = startCol + indW

  append "case "
  printExpr (unLoc scrut)
  append " of"
  newline

  withAnchor anch $ withSep newline $ map (printAlt maxWidth) matches

-- With NoAlignment, maxWidth should be 0
printAlt :: Int -> LMatch GhcPs (LHsExpr GhcPs) -> Hattier
printAlt maxWidth (L _ Match {m_pats = pats, m_grhss = grhss}) = do
  anch <- asks currentAnchor
  indentTo anch

  let patTxt = T.intercalate " " (map pprText pats)
  append $ padTo maxWidth patTxt

  printRHS ArrowSign grhss

-- * let expressions

-- | Pretty-print a @let ... in ...@ expression.
-- The style is driven by 'letAlignment' in 'Config'.
printLetExpr :: HsLocalBinds GhcPs -> LHsExpr GhcPs -> Hattier
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
printBind :: Int -> LHsBind GhcPs -> Hattier
printBind alignCol (L _ (FunBind _ lname mg)) = do
  let name = pprText $ unLoc lname
  append $ padTo alignCol name

  case unLoc (mg_alts mg) of
    [L _ Match {m_pats = pats, m_grhss = grhss}] -> do
      printPats (zip pats (repeat 0))
      printRHS EqualsSign grhss
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
printRHS :: RHSprefix -> GRHSs GhcPs (LHsExpr GhcPs) -> Hattier
printRHS prefix (GRHSs _ [L _ (GRHS _ [] body)] _) = do
  append $ case prefix of
    EqualsSign -> " = "
    ArrowSign -> " -> "
  printExpr (unLoc body)
printRHS prefix (GRHSs _ grhsList _) = do
  anch <- gets currentColumn
  withAnchor anch $ printGRHS prefix grhsList

-- ** guarded RHS

printGRHS :: RHSprefix -> [LGRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))] -> Hattier
printGRHS prefix grhss = do
  anch <- asks currentAnchor
  withSep (newline >> indentTo anch) $ map (printGuard prefix) grhss

-- | Print a single guarded RHS as @<ind>| <guards> = <body>@.
-- The body printer is passed as a parameter to avoid a circular dependency
-- between this module and the caller's body-printing logic.
printGuard :: RHSprefix -> LGRHS GhcPs (LHsExpr GhcPs) -> Hattier
printGuard prefix (L _ (GRHS _ guards body)) = do
  append " | "
  withSep (append " ") $ map fallback guards
  append $ case prefix of
    EqualsSign -> " = "
    ArrowSign -> " -> "
  printExpr (unLoc body)
