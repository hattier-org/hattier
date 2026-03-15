module Hattier.Types where

import Control.Monad.RWS (RWS, execRWS)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import GHC.Hs (GhcPs, HsModule)

type Hattier = RWS Env Log FormatterState ()

type Log = [Text]

type HattierModule = HsModule GhcPs

data Config = Config
  { indentWidth :: Int
  , maxLineLength :: Int
  }

data Env = Env
  { ast :: HattierModule
  , cfg :: Config
  }

data FormatterState = FormatterState
  { builder :: Builder -- The rendered source code so far
  }

defaultConfig :: Config
defaultConfig = Config {indentWidth = 2, maxLineLength = 80}

initialState :: FormatterState
initialState = FormatterState {builder = mempty}

execHattier :: Hattier -> Env -> FormatterState -> (Text, Log)
execHattier hat env st =
  let (finalState, logs) = execRWS hat env st
   in (toLazyText (builder finalState), logs)
