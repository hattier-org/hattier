{-# LANGUAGE RecordWildCards #-}

module Hattier.Format
  ( fmt
  ) where

import Control.Monad.State (gets, modify)
import Data.Text as T
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import Hattier.Types

fmt :: HattierMonad
fmt = sequence_ [fmtWithBuiltinGHC]

-- | Only for example purposes; please delete once proper formatters are written
fmtWithBuiltinGHC :: HattierMonad
fmtWithBuiltinGHC = do
  ast <- gets parsedModule
  let output = T.pack . showSDocUnsafe . ppr $ ast
  modify $ \s -> s {outputText = outputText s <> output}
