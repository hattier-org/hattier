-- | Pretty-printer for Haskell module structure.
--
-- Handles the top-level layout of a module: the header and module name,
-- export list, import declarations, and top-level declarations.
module Hattier.Printer.Module (printModule) where

import Control.Monad (when)
import Control.Monad.RWS
import Data.Text qualified as T
import GHC.Hs
import GHC.Types.SrcLoc
import Hattier.Printer.Combinators
import Hattier.Printer.Declaration
import Hattier.Types

-- | Print a complete Haskell module: header, imports, and top-level declarations.
printModule :: Hattier
printModule = do
  printModHeader
  printModImports
  printModDecls

-- | Print the module header (@module Foo.Bar where@), including the export
-- list if one is present. Emits nothing when the module has no name.
printModHeader :: Hattier
printModHeader = do
  source <- asks ast
  -- only print a header if there is one
  case hsmodName source of
    Nothing -> pure ()
    Just (L _ n) -> do
      append "module "
      printModName n
      -- print module exports
      case hsmodExports source of
        Nothing -> pure ()
        Just exps -> printModExports exps
      append " where"
      newline
      newline

-- | Append the string form of a 'ModuleName' to the output.
printModName :: ModuleName -> Hattier
printModName name = append (T.pack $ moduleNameString name)

-- | Print the export list of a module. Currently a no-op.
-- TODO: append exports nicely aligned
printModExports :: XRec GhcPs [LIE GhcPs] -> Hattier
printModExports _ = pure ()

-- | Print all import declarations, separated by newlines. Emits a blank line
-- after the imports block when top-level declarations follow.
printModImports :: Hattier
printModImports = do
  source <- asks ast
  case hsmodImports source of
    [] -> pure ()
    imports -> do
      withSep newline $ map printImport imports
      -- We don't print trailing newlines when there are only
      -- imports in the module (and potentially a module header
      -- and exports)
      case hsmodDecls source of
        [] -> pure ()
        _ -> newline >> newline

-- | Print a single import declaration, including the @qualified@ keyword,
-- @as@ alias, and explicit import\/hiding list when present.
printImport :: LImportDecl GhcPs -> Hattier
printImport (L _ imp) = do
  append "import "

  when (ideclQualified imp /= NotQualified) $
    append "qualified "

  fallback (unLoc (ideclName imp))

  case ideclAs imp of
    Nothing -> pure ()
    Just (L _ asName) ->
      append " as " >> fallback asName

  -- import list / hiding
  case ideclImportList imp of
    Nothing -> pure ()
    Just (hidingFlag, (L _ names)) -> do
      case hidingFlag of
        EverythingBut -> append " hiding"
        Exactly -> pure ()

      append " ("
      withSep (append ", ") $ map (fallback . unLoc) names
      append ")"

-- | Print all top-level declarations. Inserts a single newline between a type
-- signature and its value binding, and a blank line between all other adjacent
-- declarations.
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
