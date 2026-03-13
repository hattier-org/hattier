{-# LANGUAGE RecordWildCards #-}

module Hattier.Printer.Core where

import qualified Data.Text as T
import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import Hattier.Printer.Combinators
import Hattier.Types

printModHeader :: HattierModule -> Hattier
printModHeader HsModule {..} = do
  append "module "
  -- parse the module name
  case hsmodName of
    Nothing -> pure ()
    Just (L _ n) -> printModName n
  -- parse module exports
  case hsmodExports of
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

printModImports :: HattierModule -> Hattier
printModImports HsModule {..} = mapM_ printImport hsmodImports

-- TODO: append an import statement (including qualified, as, etc.)
printImport :: LImportDecl GhcPs -> Hattier
printImport (L _ ImportDecl {}) = pure ()

printModDecls :: HattierModule -> Hattier
printModDecls HsModule {..}
  -- splitting up these cases enables us to only put
  -- newlines in between declarations and not after the
  -- final one.
 =
  case hsmodDecls of
    [] -> pure ()
    d:ds -> do
      printDecl d
      mapM_ (\decl -> newline >> printDecl decl) ds

-- TODO: pretty print a declaration. you can focus on one
-- type of declaration by pattern matching, leaving the 
-- current implementation as default case at the bottom
printDecl :: LHsDecl GhcPs -> Hattier
printDecl decl = append $ T.pack . showSDocUnsafe . ppr $ decl
