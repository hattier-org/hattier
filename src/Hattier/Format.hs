-- | Thin wrapper that exposes the top-level formatting action.
module Hattier.Format
  ( fmt,
  )
where

import Hattier.Printer.Module
import Hattier.Types

-- | Format a Haskell module. Delegates to 'printModule' from
-- "Hattier.Printer.Module" to drive the full pretty-printing pipeline.
fmt :: Hattier
fmt = printModule
