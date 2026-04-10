-- | Core types for the Hattier formatter.
--
-- Defines the 'Hattier' monad — an RWS monad — together with the
-- read-only 'Env', write-only 'Log', and mutable 'FormatterState' it
-- operates over.
module Hattier.Types where

import Control.Monad.RWS (RWS, execRWS)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import GHC.Hs (GhcPs, HsModule)
import Hattier.Config
import Options.Generic hiding (Text)

-- | The formatter monad: a Reader\/Writer\/State monad over 'Env', 'Log',
-- and 'FormatterState'. All printing actions run inside this monad.
type Hattier = RWS Env Log FormatterState ()

-- | Diagnostic messages emitted during formatting (e.g. fallback warnings).
type Log = [Text]

-- | A GHC-parsed Haskell module, aliased for convenience.
type HattierModule = HsModule GhcPs

-- | The read-only environment threaded through the formatter. Holds the
-- source AST, the resolved configuration, and the current indentation anchor.
data Env = Env
  { ast :: HattierModule,
    cfg :: Config Unwrapped,
    currentAnchor :: Int -- The column that represents the current anchor to start printing from
  }

-- | The mutable state threaded through the formatter. Tracks the output
-- buffer and the column position of the next character to be written.
data FormatterState = FormatterState
  { builder :: Builder, -- The rendered source code so far
    currentColumn :: Int -- The column that the next printed item should be appended to
  }

-- | The empty 'FormatterState' used to start a fresh formatting run.
initialState :: FormatterState
initialState = FormatterState {builder = mempty, currentColumn = 0}

-- | Run a 'Hattier' action to completion and return the formatted source
-- 'Text' alongside any 'Log' messages that were emitted.
execHattier :: Hattier -> Env -> FormatterState -> (Text, Log)
execHattier hat env st =
  let (finalState, logs) = execRWS hat env st
   in (toLazyText (builder finalState), logs)
