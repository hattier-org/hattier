module Types where

import Control.Monad.RWS (RWS, execRWS)
import Data.Text         (Text)

{- NOTE(Emilia):
 - Im not actually if we'll need IO here but lets not add it prematurely for now.
 - Please redefine the SourceFiles type once we figure out how to parse things
 -}
type HattierMonad = RWS Config Log SourceFiles ()
type SourceFiles  = [Text]
type Log          = [Text]

execHattier :: HattierMonad -> Config -> SourceFiles -> (SourceFiles, Log)
execHattier = execRWS

data Config = Config
  { indentWidth   :: Int
  , maxLineLength :: Int
  }
