{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | Configuration types for Hattier and its command-line interface.
--
-- 'Config' holds all user-facing formatting options. 'CLI' wraps it with a
-- positional file argument for the executable. 'MergeFlags' provides generic
-- merging of a file-based config with CLI flag overrides.
module Hattier.Config where

import Data.Default (Default, def)
import Data.Maybe (fromJust)
import Dhall (FromDhall, Natural)
import Dhall.Marshal.Encode (ToDhall)
import GHC.Generics
import Options.Generic

-- | The top-level CLI record: a 'Config' paired with an optional file path argument.
data CLI = CLI (Config Wrapped) PosArg deriving (Generic)

-- | The optional positional file path argument on the command line.
newtype PosArg = PosArg (Maybe FilePath <?> "file name") deriving (Generic, ParseRecord)

-- | All formatting options that Hattier supports. Parameterised by @w@ so the
-- same record type can be used for both CLI-wrapped and fully-resolved values.
data Config w = Config
  { indentWidth :: w ::: Natural <#> "w" <!> "2" <?> "The desired indentation width",
    letAlignment :: w ::: Alignment <#> "l" <!> "PrimaryAlignment" <?> "The alignment style for let-bindings: PrimaryAlignment or NoAlignment",
    funcAlignment :: w ::: Alignment <#> "f" <!> "PrimaryAlignment" <?> "The alignment style for function declarations: PrimaryAlignment or NoAlignment",
    caseAlignment :: w ::: Alignment <#> "c" <!> "PrimaryAlignment" <?> "The alignment style for case expressions: PrimaryAlignment or NoAlignment",
    inPlace :: w ::: Bool <#> "i" <!> "false" <?> "edit files in place",
    version :: w ::: Bool <#> "v" <!> "false" <?> "hattier version",
    defaultConfig :: w ::: Bool <#> "d" <!> "false" <?> "Print out the default configuration file"
  }
  deriving (Generic)

-- | Controls how declaration names or patterns are padded to achieve visual alignment.
data Alignment
  = -- | Indent each binding uniformly, no padding
    NoAlignment
  | -- | Pad names so '=' signs align to the longest name
    PrimaryAlignment
  deriving (Eq, Generic, Read, Show, FromDhall, ToDhall, ParseField, ParseFields, ParseRecord)

instance ParseRecord CLI where
  parseRecord = CLI <$> parseRecord <*> parseRecord

instance Show (Config Unwrapped) where
  show Config {..} =
    "let Alignment = < PrimaryAlignment | NoAlignment > in\n"
      <> "{ indentWidth = "
      <> show indentWidth
      <> "\n, letAlignment = Alignment."
      <> show letAlignment
      <> "\n, funcAlignment = Alignment."
      <> show funcAlignment
      <> "\n, caseAlignment = Alignment."
      <> show caseAlignment
      <> "\n, inPlace = "
      <> show inPlace
      <> "\n, version = "
      <> show version
      <> "\n, defaultConfig = "
      <> show defaultConfig
      <> "\n}"

instance ParseRecord (Config Wrapped)

deriving instance FromDhall (Config Unwrapped)

deriving instance ToDhall (Config Unwrapped)

instance Default (Config Unwrapped) where
  def = fromJust $ unwrapRecordPure []

----------------------------------------
--- Generic Merging of record fields ---
----------------------------------------
-- | Merge three 'Config'-shaped values: defaults, a base (e.g. from a config
-- file), and flag overrides (from the CLI). A flag value takes precedence over
-- the base only when it differs from the default.
class MergeFlags a where
  mergeFlags :: a -> a -> a -> a
  default mergeFlags :: (Generic a, GMerge (Rep a)) => a -> a -> a -> a
  mergeFlags dflt base flags = to (gmerge (from dflt) (from base) (from flags))

class GMerge f where
  gmerge :: f p -> f p -> f p -> f p

instance (GMerge f) => GMerge (M1 i c f) where
  gmerge (M1 d) (M1 b) (M1 f) = M1 (gmerge d b f)

instance (GMerge a, GMerge b) => GMerge (a :*: b) where
  gmerge (d1 :*: d2) (b1 :*: b2) (f1 :*: f2) =
    gmerge d1 b1 f1 :*: gmerge d2 b2 f2

instance (Eq a) => GMerge (K1 i a) where
  gmerge (K1 dflt) (K1 base) (K1 flag)
    | flag == dflt = K1 base
    | otherwise = K1 flag

instance MergeFlags (Config Unwrapped)
