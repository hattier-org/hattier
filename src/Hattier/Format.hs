{-# LANGUAGE RecordWildCards #-}

module Hattier.Format
  ( fmt
  ) where

import Hattier.Printer.Core
import Hattier.Types

fmt :: HattierModule -> Hattier
fmt ast = do
  printModHeader ast
  printModImports ast
  printModDecls ast
