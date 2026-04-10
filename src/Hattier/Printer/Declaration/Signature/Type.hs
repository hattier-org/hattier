-- | Printer for Haskell type expressions within signatures.
--
-- Handles the full range of 'HsType' forms: type variables, function arrows,
-- applications, tuples, lists, foralls, operators, and more.
module Hattier.Printer.Declaration.Signature.Type
  ( printTypeSig,
  )
where

import GHC.Core.Type
import GHC.Hs
import GHC.Types.SrcLoc
import Hattier.Printer.Combinators
import Hattier.Types

-- | Print a type signature of the form @name1, name2 :: type@.
printTypeSig :: [LIdP GhcPs] -> LHsSigWcType GhcPs -> Hattier
printTypeSig names (HsWC {hswc_body = L _ sig}) = do
  printNames names
  append " :: "
  printHsSigType sig

-- | Print a comma-separated list of names, as they appear on the left-hand
-- side of a type signature.
printNames :: [LIdP GhcPs] -> Hattier
printNames [] = pure ()
printNames [(L _ name)] = fallback name
printNames ((L _ name) : rest) = do
  fallback name
  append ", "
  printNames rest

-- | Print a 'HsSigType', emitting the @forall@ binder if one is present
-- before delegating the body to 'printLHsType'.
printHsSigType :: HsSigType GhcPs -> Hattier
printHsSigType (HsSig {sig_bndrs = outerForall, sig_body = body}) = do
  case outerForall of
    HsOuterImplicit {} -> pure ()
    HsOuterExplicit _ [] -> pure ()
    HsOuterExplicit _ vars -> do
      append "forall "
      printForallVars vars
      append ". "

  printLHsType body

-- | Print a space-separated list of @forall@ type variable binders.
printForallVars :: [LHsTyVarBndr Specificity (NoGhcTc GhcPs)] -> Hattier
printForallVars [] = pure ()
printForallVars (var : vars) = do
  fallback var
  append " "
  printForallVars vars

-- | Print a located 'HsType', dispatching on every supported constructor.
-- Falls back to 'ppr' for any form not yet handled explicitly.
printLHsType :: LHsType GhcPs -> Hattier
printLHsType (L _ ty) = case ty of
  HsTyVar _ _ (L _ name) -> fallback name
  HsAppTy _ f a -> do
    printLHsType f
    append " "
    printLHsType a
  HsAppKindTy _ t k -> do
    printLHsType t
    append " "
    fallback k
  HsFunTy _ _ arg res -> do
    printLHsType arg
    append " -> "
    printLHsType res
  HsListTy _ t -> do
    append "["
    printLHsType t
    append "]"
  HsTupleTy _ _ ts -> do
    append "("
    printNestedStructure ts
    append ")"
  HsSumTy _ ts -> do
    append "(#"
    printNestedStructure ts
    append "#)"
  HsOpTy _ _ l op r -> do
    printLHsType l
    append " "
    fallback op
    append " "
    printLHsType r
  HsParTy _ t -> do
    append "("
    printLHsType t
    append ")"
  HsStarTy _ _ -> append "*"
  HsExplicitListTy _ _ ts -> do
    append "["
    printNestedStructure ts
    append "]"
  HsExplicitTupleTy _ ts -> do
    -- We currently don't print a promoted tuple with a tick (') before it
    -- because every tuple in type signatures is seen as promoted by the
    -- GHC parser. This might be a bug in Hattier/Parser.hs.
    append "("
    printNestedStructure ts
    append ")"
  HsTyLit _ lit -> fallback lit
  HsWildCardTy _ -> append "_"
  HsBangTy _ _ t -> do append "!"; printLHsType t
  -- TODO:
  -- HsForAllTy _ (HsForAllVis _ _bndrs) t -> do
  --   append "forall "
  --   -- something like: printBndrs bndrs
  --   printLHsType t
  -- HsForAllTy _ (HsForAllInvis _ _bndrs) t -> do
  --   append "forall "
  --   -- something like: printBndrs bndrs
  --   printLHsType t
  -- HsQualTy _ _ _ -> undefined
  -- HsIParamTy _ _ _ -> undefined
  -- HsKindSig _ _ _ -> undefined
  -- HsSpliceTy _ _ -> undefined
  -- HsDocTy _ _ _ -> undefined
  -- HsRecTy _ _ -> undefined
  -- XHsType _ -> undefined
  _ -> fallback ty

-- | Print a comma-separated sequence of types, as found inside tuples,
-- unboxed sums, and explicit type-level lists.
printNestedStructure :: [LHsType GhcPs] -> Hattier
printNestedStructure [] = pure ()
printNestedStructure (t : ts) = do
  printLHsType t
  case ts of
    [] -> pure ()
    _ -> append ", "
  printNestedStructure ts
