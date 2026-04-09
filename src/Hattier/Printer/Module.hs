module Hattier.Printer.Module (printModule) where

import Control.Monad.RWS
import Data.Text qualified as T
import GHC.Hs
import GHC.Types.SrcLoc
import Hattier.Printer.Combinators
import Hattier.Printer.Declaration
import Hattier.Types

printModule :: Hattier
printModule = do
  printModHeader
  printModImports
  printModDecls

printModHeader :: Hattier
printModHeader = do
  source <- asks ast
  -- only print a header if there is one
  case hsmodName source of
    Nothing -> pure ()
    Just (L _ n) -> do
      append "module "
      printModName n
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
printModDecls = do
  source <- asks ast
  -- splitting up these cases enables us to only put
  -- newlines in between declarations and not after the
  -- final one.
  case hsmodDecls source of
    [] -> pure ()
    d : ds -> do
      printDecl d
      -- Make sure that type signatures "stick to" their respective
      -- declarations
      mapM_
        ( \(prev, cur) -> case prev of
            L _ (SigD _ _) -> newline >> printDecl cur
            _ -> newline >> newline >> printDecl cur
        )
        (zip (d : ds) ds)
