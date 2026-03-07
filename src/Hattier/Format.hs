{-# LANGUAGE RecordWildCards #-}

module Hattier.Format
  ( fmt
  ) where

import Control.Monad.State (get, put)
import Hattier.Types

fmt :: Hattier
fmt = sequence_ [fmtNothing]

-- | Only for example purposes; please delete once proper formatters are written
fmtNothing :: Hattier
fmtNothing = do
  ast <- get
  put ast
