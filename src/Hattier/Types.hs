module Hattier.Types where

import Control.Monad.RWS (RWS, execRWS)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import GHC.Hs (GhcPs, HsModule)

type Hattier = RWS Config Log FormatterState ()

type Log = [Text]

type HattierModule = HsModule GhcPs

data Config = Config
  { indentWidth :: Int
  , maxLineLength :: Int
  }

data FormatterState = FormatterState
  { currentIndent :: !Int
  , currentColumn :: !Int
  , builder :: Builder -- The rendered source code so far
  }

defaultConfig :: Config
defaultConfig = Config {indentWidth = 2, maxLineLength = 80}

initialState :: FormatterState
initialState =
  FormatterState {currentIndent = 0, currentColumn = 0, builder = mempty}

execHattier :: Hattier -> Config -> FormatterState -> (Text, Log)
execHattier hat cfg st =
  let (finalState, logs) = execRWS hat cfg st
   in (toLazyText (builder finalState), logs)
