-- | Printer for value-level declarations (function and pattern bindings).
module Hattier.Printer.Declaration.Value
  ( printValueDecl,
  )
where

import GHC.Hs
import GHC.Types.SrcLoc
import Hattier.Printer.Combinators
import Hattier.Printer.Declaration.Value.Function
import Hattier.Types

-- | Print a value declaration. Currently handles 'FunBind'; falls back to
-- 'ppr' for other binding forms (e.g. 'PatBind').
printValueDecl :: HsBind GhcPs -> Hattier
-- function declarations
printValueDecl (FunBind {fun_id = L _ name, fun_matches = MG _ (L _ matches)}) =
  printFunBind name matches
printValueDecl bind = fallback bind
