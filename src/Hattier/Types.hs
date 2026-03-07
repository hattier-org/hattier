module Hattier.Types where

import Control.Monad.RWS (RWS, execRWS)
import Data.Text (Text)
import GHC.Hs (GhcPs, HsModule)

type Hattier = RWS Config Log FormatterState ()

type Log = [Text]

type HattierModule = HsModule GhcPs

data Config = Config
  { indentWidth :: Int
  , maxLineLength :: Int
  }

type FormatterState = HattierModule

execHattier :: Hattier -> Config -> FormatterState -> (FormatterState, Log)
execHattier = execRWS
