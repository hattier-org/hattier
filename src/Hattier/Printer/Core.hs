{-# LANGUAGE RecordWildCards #-}

module Hattier.Printer.Core where

import Control.Monad.RWS
import qualified Data.Text as T
import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import Hattier.Printer.Combinators
import Hattier.Types

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
printDecl decl = append $ T.pack . showSDocUnsafe . ppr $ decl
