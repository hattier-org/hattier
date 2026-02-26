{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
module Types where

import Dhall (FromDhall, Natural)
import Options.Generic ( Generic, ParseRecord, Unwrapped, Wrapped, (:::) , type (<!>) , type (<?>) )
import Control.Monad.RWS (RWS, execRWS)
import Data.Text         (Text)

{- NOTE(Emilia):
 - Im not actually if we'll need IO here but lets not add it prematurely for now.
 - Please redefine the SourceFiles type once we figure out how to parse things
 -}
type HattierMonad = RWS (Config Unwrapped) Log SourceFiles ()
type SourceFiles  = [Text]
type Log          = [Text]

execHattier :: HattierMonad -> (Config Unwrapped) -> SourceFiles -> (SourceFiles, Log)
execHattier = execRWS

data Config w = Config
  { indentWidth   :: w ::: Natural <!> "2"   <?> "The desired indentation width"
  , maxLineLength :: w ::: Natural <!> "130" <?> "Number of characters before a forced line break"
  } deriving (Generic)

instance ParseRecord        (Config Wrapped)
deriving instance Show      (Config Unwrapped)
deriving instance FromDhall (Config Unwrapped)
