module Hattier.Printer.Declaration.Value
  ( printValueDecl,
  )
where

import Control.Monad.RWS
import Data.Text qualified as T
import GHC.Hs
import GHC.Types.SrcLoc
import Hattier.Config
import Hattier.Printer.Combinators
import Hattier.Printer.Declaration.Value.Function
import Hattier.Printer.Declaration.Value.Let
import Hattier.Types

printValueDecl :: HsBind GhcPs -> Hattier
-- let declarations
printValueDecl
  ( FunBind
      { fun_id = L _ name,
        fun_matches = MG _ (L _ [L _ (Match _ _ _ (GRHSs _ [L _ (GRHS _ [] (L _ (HsLet _ binds body)))] _))])
      }
    ) = do
    indW <- asks (fromIntegral . indentWidth . cfg)
    let ind = T.replicate indW " "
    append (pprText name) >> append " ="
    newline >> append ind
    printLetExpr binds body
-- function declarations
printValueDecl (FunBind {fun_id = L _ name, fun_matches = MG _ (L _ matches)}) =
  printFunBind name matches
printValueDecl bind = append $ pprText bind
