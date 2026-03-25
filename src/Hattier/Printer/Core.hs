{-# LANGUAGE RecordWildCards #-}

module Hattier.Printer.Core where

import Control.Monad.RWS
import qualified Data.Text as T
import GHC.Hs
import GHC.Types.SrcLoc
import Hattier.Format.Let (printLetExpr)
import Hattier.Printer.Combinators
import Hattier.Types
import Hattier.Config

printModHeader :: Hattier
printModHeader = do
  source <- asks ast
  append "module "
  -- parse the module name
  case hsmodName source of
    Nothing -> pure ()
    Just (L _ n) -> printModName n
  -- parse module exports
  case hsmodExports source of
    Nothing -> pure ()
    Just exps -> printModExports exps
  append " where"
  newline
  newline

printModName :: ModuleName -> Hattier
printModName name = append (T.pack $ moduleNameString name)

-- TODO: append exports nicely aligned
printModExports :: XRec GhcPs [LIE GhcPs] -> Hattier
printModExports _ = pure ()

printModImports :: Hattier
printModImports = do
  source <- asks ast
  mapM_ printImport (hsmodImports source)

-- TODO: append an import statement (including qualified, as, etc.)
printImport :: LImportDecl GhcPs -> Hattier
printImport (L _ ImportDecl {}) = pure ()

printModDecls :: Hattier
printModDecls
  -- splitting up these cases enables us to only put
  -- newlines in between declarations and not after the
  -- final one.
 = do
  source <- asks ast
  case hsmodDecls source of
    [] -> pure ()
    d:ds -> do
      printDecl d
      mapM_ (\decl -> newline >> printDecl decl) ds

-- TODO: pretty print a declaration. you can focus on one
-- type of declaration by pattern matching, leaving the 
-- current implementation as default case at the bottom
printDecl :: LHsDecl GhcPs -> Hattier
-- Let expressions:
printDecl (L _ (ValD _ (FunBind _ lname mg)))
  -- A top-level binding whose sole RHS is a let expression
  | [L _ (Match _ _ _pats (GRHSs _ [L _ (GRHS _ [] (L _ (HsLet _ binds body)))] _))] <-
      unLoc (mg_alts mg) = do
      indW <- asks (fromIntegral . indentWidth . cfg)
      let ind = T.replicate indW " "
      append (pprText (unLoc lname)) >> append " ="
      newline >> append ind
      printLetExpr binds body
-- Default case: just print the declaration as-is
printDecl decl = append $ pprText decl
