module Hattier.Printer.Declaration.Value
  ( printValueDecl,
  )
where

import GHC.Hs
import GHC.Types.SrcLoc
import Hattier.Printer.Combinators
import Hattier.Printer.Declaration.Value.Function
import Hattier.Types

printValueDecl :: HsBind GhcPs -> Hattier
-- function declarations
printValueDecl (FunBind {fun_id = L _ name, fun_matches = MG _ (L _ matches)}) =
  printFunBind name matches
printValueDecl bind = append $ pprText bind
