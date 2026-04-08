module Hattier.Printer.Declaration.Value.Let
  ( printLetExpr,
  )
where

import Control.Monad.RWS
import Data.Text qualified as T
import GHC.Data.Bag (bagToList)
import GHC.Hs
import GHC.Types.SrcLoc
import Hattier.Config
import Hattier.Printer.Combinators
import Hattier.Printer.Declaration.Value.Guard
import Hattier.Printer.Expression
import Hattier.Types

-- | Pretty-print a @let ... in ...@ expression.
-- The style is driven by 'letAlignment' in 'Config'.
printLetExpr :: HsLocalBinds GhcPs -> LHsExpr GhcPs -> Hattier
printLetExpr (HsValBinds _ (ValBinds _ binds _sigs)) body = do
  style <- asks (letAlignment . cfg)
  indW <- asks (fromIntegral . indentWidth . cfg)
  let bindList = bagToList binds
  case style of
    NoAlignment -> do
      let bindInd = T.replicate (indW + 4) " "
      let inInd = T.replicate indW " "
      append "let "
      printBinds bindInd 0 bindList
      newline >> append inInd >> append "in " >> printLetBody (unLoc body)
    PrimaryAlignment -> do
      let bindInd = T.replicate (indW + 4) " "
      let inInd = T.replicate indW " "
      let alignCol = bindAlignCol bindList
      append "let "
      printBinds bindInd alignCol bindList
      newline >> append inInd >> append "in  " >> printLetBody (unLoc body)
printLetExpr localBinds body = do
  -- TODO: HsIPBinds and EmptyLocalBinds cases
  indW <- asks (fromIntegral . indentWidth . cfg)
  let ind = T.replicate indW " "
  append $ pprText localBinds
  newline >> append ind >> append "in " >> printLetBody (unLoc body)

-- | Print a list of bindings multi-line, using @alignCol@ for padding.
-- Pass @alignCol = 0@ for no alignment.
--
--   let x        = 1   -- PrimaryAlignment (alignCol = 8)
--       longName = 2
--
--   let x = 1          -- NoAlignment (alignCol = 0)
--       longName = 2
printBinds :: T.Text -> Int -> [LHsBind GhcPs] -> Hattier
printBinds _ _ [] = pure ()
printBinds ind alignCol (b : bs) = do
  printBind ind alignCol b
  mapM_ (\bind -> newline >> append ind >> printBind ind alignCol bind) bs

-- | Print a single binding, padding the name to @alignCol@ columns.
-- When @alignCol = 0@ no padding is added.
printBind :: T.Text -> Int -> LHsBind GhcPs -> Hattier
printBind bindInd alignCol (L _ (FunBind _ lname mg)) = do
  let name = pprText $ unLoc lname
  let padding = T.replicate (max 0 (alignCol - T.length name)) " "
  append name >> append padding
  case unLoc (mg_alts mg) of
    [L _ (Match _ _ pats (GRHSs _ grhsList _))] ->
      case grhsList of
        [L _ (GRHS _ [] body)] -> do
          -- Unguarded: emit patterns then " = " then body
          mapM_ (\p -> append " " >> append (pprText p)) pats
          append " = "
          printLetBody (unLoc body)
        _ -> do
          -- Guarded: emit patterns then each guard on its own line
          mapM_ (\p -> append " " >> append (pprText p)) pats
          mapM_ (\grhs -> newline >> printGuard bindInd printLetBody grhs) grhsList
    -- Empty or multi-clause let bindings are not valid Haskell and
    -- cannot be produced by GHC's parser, so these branches are unreachable.
    -- still, we need to handle them to satisfy the type checker.
    _ -> pure ()
printBind _ _ bind =
  -- TODO: PatBind and other binding forms
  append $ pprText $ unLoc bind

-- | Compute the column at which @=@ should appear for 'PrimaryAlignment':
-- the length of the longest binding name.  Returns 0 for an empty list.
bindAlignCol :: [LHsBind GhcPs] -> Int
bindAlignCol [] = 0
bindAlignCol bs = maximum (map nameLen bs)
  where
    nameLen :: LHsBind GhcPs -> Int
    nameLen (L _ (FunBind _ lname _)) = T.length $ pprText $ unLoc lname
    nameLen _ = 0

-- | Print the body of a @let@ expression, recursing into nested @let@s.
printLetBody :: HsExpr GhcPs -> Hattier
printLetBody (HsLet _ nestedBinds nestedBody) =
  printLetExpr nestedBinds nestedBody
printLetBody expr = printExpr expr
