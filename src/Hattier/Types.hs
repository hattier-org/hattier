module Hattier.Types where

import Control.Monad.RWS (RWS, execRWS)
import Data.Text (Text)
import GHC.Hs (GhcPs, HsModule)

type HattierMonad = RWS Config Log FormatterState ()

type Log = [Text]

type HattierModule = HsModule GhcPs

data Config = Config
  { indentWidth :: Int
  , maxLineLength :: Int
  }

data FormatterState = FormatterState
  { parsedModule :: HattierModule
  , outputText :: Text -- accumulated formatted text
  }

execHattier :: HattierMonad -> Config -> FormatterState -> (Text, Log)
execHattier m config initialState =
  let (finalState, logs) = execRWS m config initialState
   in (outputText finalState, logs)
