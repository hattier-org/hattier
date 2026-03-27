module Hattier.Printer.Declaration
  ( printDecl,
  )
where

import GHC.Hs
import GHC.Types.SrcLoc
import Hattier.Printer.Combinators
import Hattier.Printer.Declaration.Value
import Hattier.Types

printDecl :: LHsDecl GhcPs -> Hattier
printDecl (L _ (ValD _ bind)) = printValueDecl bind
printDecl decl = append $ pprText decl -- NOTE: default fallback
