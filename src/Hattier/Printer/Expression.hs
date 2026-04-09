module Hattier.Printer.Expression
  ( printExpr,
    printRHS,
    printLetExpr, -- We only export this because we have isolated tests for let-bindings
  )
where

import Control.Monad.RWS
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.Bag (bagToList)
import GHC.Hs
import GHC.Types.SrcLoc
import Hattier.Config
import Hattier.Printer.Combinators
import Hattier.Printer.Pattern
import Hattier.Printer.Utils
import Hattier.Types

printExpr :: HsExpr GhcPs -> Hattier
printExpr (HsCase _ scrut mg) = printCaseExpr scrut mg
printExpr (HsLet _ binds body) = printLetExpr binds body
printExpr expr = fallback expr

-- * case expressions

printCaseExpr :: LHsExpr GhcPs -> MatchGroup GhcPs (LHsExpr GhcPs) -> Hattier
printCaseExpr scrut (MG _ (L _ matches)) = do
  append "case "
  printExpr (unLoc scrut)
  append " of"
  newline

  indW <- asks (fromIntegral . indentWidth . cfg)
  style <- asks (caseAlignment . cfg)

  let widths = map matchWidth matches
      maxWidth = computeAlignment style widths

  withSep newline $ map (printAlt (indToText indW) maxWidth) matches

-- With NoAlignment, maxWidth should be 0
printAlt :: Text -> Int -> LMatch GhcPs (LHsExpr GhcPs) -> Hattier
printAlt ind maxWidth (L _ Match {m_pats = pats, m_grhss = grhss}) = do
  let patTxt = T.intercalate " " (map pprText pats)
  append ind
  append $ padTo maxWidth patTxt
  append " -> "

  case grhss of
    GRHSs _ [L _ (GRHS _ [] body)] _ ->
      printExpr (unLoc body)
    GRHSs _ xs _ ->
      printGRHS ind xs

-- * let expressions

-- | Pretty-print a @let ... in ...@ expression.
-- The style is driven by 'letAlignment' in 'Config'.
printLetExpr :: HsLocalBinds GhcPs -> LHsExpr GhcPs -> Hattier
printLetExpr (HsValBinds _ (ValBinds _ binds _sigs)) body = do
  style <- asks (letAlignment . cfg)
  indW <- asks (fromIntegral . indentWidth . cfg)
  let bindList = bagToList binds
      bindInd = indToText (indW + 4)
      alignCol = computeAlignment style (map nameLen bindList)
  append "let "
  printBinds bindInd alignCol bindList
  newline >> append (indToText indW) >> append "in  " >> printExpr (unLoc body)
printLetExpr localBinds body = do
  -- TODO: HsIPBinds and EmptyLocalBinds cases
  indW <- asks (fromIntegral . indentWidth . cfg)
  fallback localBinds
  newline >> append (indToText indW) >> append "in  " >> printExpr (unLoc body)

-- | Print a list of bindings multi-line, using @alignCol@ for padding.
-- Pass @alignCol = 0@ for no alignment.
--
--   let x        = 1   -- PrimaryAlignment (alignCol = 8)
--       longName = 2
--
--   let x = 1          -- NoAlignment (alignCol = 0)
--       longName = 2
printBinds :: Text -> Int -> [LHsBind GhcPs] -> Hattier
printBinds ind alignCol binds =
  withSep (newline >> append ind) $ map (printBind ind alignCol) binds

-- | Print a single binding, padding the name to @alignCol@ columns.
-- When @alignCol = 0@ no padding is added.
printBind :: Text -> Int -> LHsBind GhcPs -> Hattier
printBind bindInd alignCol (L _ (FunBind _ lname mg)) = do
  let name = pprText $ unLoc lname
  append $ padTo alignCol name
  case unLoc (mg_alts mg) of
    [L _ Match {m_pats = pats, m_grhss = grhss}] -> do
      printPats (zip pats (repeat 0))
      printRHS bindInd grhss
    -- Empty or multi-clause let bindings are not valid Haskell and
    -- cannot be produced by GHC's parser, so these branches are unreachable.
    -- still, we need to handle them to satisfy the type checker.
    _ -> pure ()
printBind _ _ bind =
  -- TODO: PatBind and other binding forms
  fallback (unLoc bind)

-- * RHS

-- | Print the RHS of a function clause or let-binding.
-- Handles the "= body" unguarded case (with special layout for let),
-- and the "| guard = body" guarded case.
printRHS :: Text -> GRHSs GhcPs (LHsExpr GhcPs) -> Hattier
printRHS ind (GRHSs _ [L _ (GRHS _ [] body)] _) = do
  case unLoc body of
    HsLet _ binds letBody ->
      append " =" >> newline >> append ind >> printLetExpr binds letBody
    _ -> append " = " >> printExpr (unLoc body)
printRHS ind (GRHSs _ grhsList _) =
  printGRHS ind grhsList

-- ** guarded RHS

printGRHS :: Text -> [LGRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))] -> Hattier
printGRHS ind grhss = mapM_ (\grhs -> newline >> printGuard ind grhs) grhss

-- | Print a single guarded RHS as @<ind>| <guards> = <body>@.
-- The body printer is passed as a parameter to avoid a circular dependency
-- between this module and the caller's body-printing logic.
printGuard :: Text -> LGRHS GhcPs (LHsExpr GhcPs) -> Hattier
printGuard ind (L _ (GRHS _ guards body)) = do
  append ind >> append "| "
  mapM_ (\g -> fallback g >> append " ") guards
  append "= "
  printExpr (unLoc body)
