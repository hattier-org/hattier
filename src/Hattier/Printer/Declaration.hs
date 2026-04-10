-- | Top-level dispatcher for printing Haskell declarations.
module Hattier.Printer.Declaration
  ( printDecl,
  )
where

import GHC.Hs
import GHC.Types.SrcLoc
import Hattier.Printer.Combinators
import Hattier.Printer.Declaration.Signature
import Hattier.Printer.Declaration.Value
import Hattier.Types

-- | Print a single top-level declaration. Dispatches to the appropriate
-- sub-printer for value bindings and type signatures; falls back to 'ppr'
-- for unsupported declaration forms.
printDecl :: LHsDecl GhcPs -> Hattier
printDecl (L _ (ValD _ bind)) = printValueDecl bind
printDecl (L _ (SigD _ sig)) = printSignature sig
printDecl decl = fallback decl
