module Hattier.Format
  ( fmt,
  )
where

import Hattier.Printer.Core
import Hattier.Types

fmt :: Hattier
fmt = do
  printModHeader
  printModImports
  printModDecls
