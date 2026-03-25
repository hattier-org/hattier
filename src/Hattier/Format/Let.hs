module Hattier.Format.Let where

import Control.Monad.RWS
import qualified Data.Text as T
import GHC.Data.Bag (bagToList)
import GHC.Hs
import GHC.Types.SrcLoc
import Hattier.Printer.Combinators
import Hattier.Types
import Hattier.Config

-- | Pretty-print a @let ... in ...@ expression.
-- The style is driven by 'letAlignment' in 'Config'.
printLetExpr :: HsLocalBinds GhcPs -> LHsExpr GhcPs -> Hattier
printLetExpr (HsValBinds _ (ValBinds _ binds _sigs)) body = do
  style    <- asks (letAlignment . cfg)
  indW     <- asks (fromIntegral . indentWidth . cfg)
  let bindList = bagToList binds
  case style of
    OneLine          -> printOneLineBinds bindList >> append " in " >> printLetBody (unLoc body)
    NoAlignment      -> do
      let bindInd = T.replicate (indW + 4) " "
      let inInd   = T.replicate indW " "
      append "let "
      printBinds bindInd 0 bindList
      newline >> append inInd >> append "in " >> printLetBody (unLoc body)
    PrimaryAlignment -> do
      let bindInd  = T.replicate (indW + 4) " "
      let inInd    = T.replicate indW " "
      let alignCol = bindAlignCol bindList
      append "let "
      printBinds bindInd alignCol bindList
      newline >> append inInd >> append "in " >> printLetBody (unLoc body)

printLetExpr localBinds body = do
  -- TODO: HsIPBinds and EmptyLocalBinds cases
  indW <- asks (fromIntegral . indentWidth . cfg)
  let ind = T.replicate indW " "
  append $ pprText localBinds
  newline >> append ind >> append "in " >> printLetBody (unLoc body)

-- | @OneLine@: all bindings on one line separated by @; @.
--
--   let x = 1; longName = 2 in body
printOneLineBinds :: [LHsBind GhcPs] -> Hattier
printOneLineBinds []     = pure ()
printOneLineBinds (b:bs) = do
  append "let "
  printBind 0 b
  mapM_ (\bind -> append "; " >> printBind 0 bind) bs

-- | Print a list of bindings multi-line, using @alignCol@ for padding.
-- Pass @alignCol = 0@ for no alignment.
--
--   let x        = 1   -- PrimaryAlignment (alignCol = 8)
--       longName = 2
--
--   let x = 1          -- NoAlignment (alignCol = 0)
--       longName = 2
printBinds :: T.Text -> Int -> [LHsBind GhcPs] -> Hattier
printBinds _   _        []     = pure ()
printBinds ind alignCol (b:bs) = do
  printBind alignCol b
  mapM_ (\bind -> newline >> append ind >> printBind alignCol bind) bs

-- | Print a single binding, padding the name to @alignCol@ columns.
-- When @alignCol = 0@ no padding is added.
printBind :: Int -> LHsBind GhcPs -> Hattier
printBind alignCol (L _ (FunBind _ lname mg)) = do
  let name    = pprText $ unLoc lname
  let padding = T.replicate (max 0 (alignCol - T.length name)) " "
  append name >> append padding >> append " = "
  case unLoc (mg_alts mg) of
    [L _ (Match _ _ [] (GRHSs _ [L _ (GRHS _ [] body)] _))] ->
      -- Simple case: no patterns, no guards
      printLetBody (unLoc body)
    _ ->
      -- TODO: patterns and guards
      append "..."
printBind _ bind =
  -- TODO: PatBind and other binding forms
  append $ pprText $ unLoc bind

-- | Compute the column at which @=@ should appear for 'PrimaryAlignment':
-- the length of the longest binding name.  Returns 0 for an empty list.
bindAlignCol :: [LHsBind GhcPs] -> Int
bindAlignCol [] = 0
bindAlignCol bs = maximum (map nameLen bs)
  where
    nameLen :: LHsBind GhcPs -> Int
    nameLen (L _ (FunBind _ lname _)) =
      T.length $ pprText $ unLoc lname
    nameLen _ = 0

-- | Print the body of a @let@ expression, recursing into nested @let@s.
printLetBody :: HsExpr GhcPs -> Hattier
printLetBody (HsLet _ nestedBinds nestedBody) =
  printLetExpr nestedBinds nestedBody
printLetBody expr =
  -- TODO: here we kind of need to recurse into normal printing... 
  append $ pprText expr
