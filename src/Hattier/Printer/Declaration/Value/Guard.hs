module Hattier.Printer.Declaration.Value.Guard
  ( printGuard,
  )
where

import Data.Text qualified as T
import GHC.Hs
import GHC.Types.SrcLoc
import Hattier.Printer.Combinators
import Hattier.Types

-- | Print a single guarded RHS as @\\n<ind>| <guards> = <body>@.
-- The body printer is passed as a parameter to avoid a circular dependency
-- between this module and the caller's body-printing logic.
printGuard :: T.Text -> (HsExpr GhcPs -> Hattier) -> LGRHS GhcPs (LHsExpr GhcPs) -> Hattier
printGuard ind bodyPrinter (L _ (GRHS _ guards body)) = do
  newline >> append ind >> append "| "
  mapM_ (\g -> append (pprText g) >> append " ") guards
  append "= "
  bodyPrinter (unLoc body)
