{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Hattier.Config where

import Data.Default (Default, def)
import Data.Maybe (fromJust)
import Data.Proxy
import Dhall (FromDhall (..), Natural, ToDhall (..))
import qualified Dhall
import qualified Dhall.Core as Dhall
import qualified Dhall.Marshal.Encode as Dhall ()
import GHC.Generics
import Options.Generic

------------------
-- Mode tagging --
------------------

data Mode = Ephemeral | Persistant

newtype (<@>) field (mode :: Mode) = ModeTag {unModeTag :: field}
  deriving (Generic, Show)

infixl 1 <@>

deriving instance (Eq a) => Eq (a <@> m)

instance (ParseField a) => ParseField (a <@> m) where
  parseField h mname c d = ModeTag <$> parseField h mname c d
  readField = ModeTag <$> readField
  metavar _ = metavar (Proxy :: Proxy a)

instance (ParseFields a) => ParseFields (a <@> m) where
  parseFields h mname c d = ModeTag <$> parseFields h mname c d

instance (ParseFields a) => ParseRecord (a <@> m)

instance (FromDhall a) => FromDhall (a <@> 'Persistant) where
  autoWith norm = ModeTag <$> autoWith norm

instance (ToDhall a) => ToDhall (a <@> 'Persistant) where
  injectWith norm =
    let enc = injectWith norm
     in Dhall.Encoder
          { Dhall.embed = \(ModeTag x) -> Dhall.embed enc x,
            Dhall.declared = Dhall.declared enc
          }

instance (Default a) => FromDhall (a <@> Ephemeral) where
  autoWith _ =
    Dhall.Decoder
      { Dhall.extract = \_ -> pure (ModeTag def),
        Dhall.expected = pure (Dhall.Record mempty)
      }

instance ToDhall (a <@> 'Ephemeral) where
  injectWith _ =
    Dhall.Encoder
      { Dhall.embed = const (Dhall.RecordLit mempty),
        Dhall.declared = Dhall.Record mempty
      }

----------------------------------------
-- CLI wrapper
----------------------------------------

data CLI = CLI (Config Wrapped) PosArg deriving (Generic)

newtype PosArg = PosArg (Maybe FilePath <?> "file name") deriving (Generic, ParseRecord)

data Config w = Config
  { indentWidth :: w ::: (Natural <@> 'Persistant) <#> "w" <!> "2" <?> "The desired indentation width",
    letAlignment :: w ::: (Alignment <@> 'Persistant) <#> "l" <!> "PrimaryAlignment" <?> "The alignment style for let-bindings: PrimaryAlignment or NoAlignment",
    funcAlignment :: w ::: (Alignment <@> 'Persistant) <#> "f" <!> "PrimaryAlignment" <?> "The alignment style for function declarations: PrimaryAlignment or NoAlignment",
    caseAlignment :: w ::: (Alignment <@> 'Persistant) <#> "c" <!> "PrimaryAlignment" <?> "The alignment style for case expressions: PrimaryAlignment or NoAlignment",
    inPlace :: w ::: (Bool <@> 'Ephemeral) <#> "i" <!> "false" <?> "edit files in place",
    version :: w ::: (Bool <@> 'Ephemeral) <#> "v" <!> "false" <?> "hattier version",
    defaultConfig :: w ::: (Bool <@> 'Ephemeral) <#> "d" <!> "false" <?> "Print out the default configuration file"
  }
  deriving (Generic)

data Alignment
  = -- | Indent each binding uniformly, no padding
    NoAlignment
  | -- | Pad names so '=' signs align to the longest name
    PrimaryAlignment
  deriving (Eq, Generic, Read, Show, FromDhall, ToDhall, ParseField, ParseFields, ParseRecord)

instance ParseRecord CLI where
  parseRecord = CLI <$> parseRecord <*> parseRecord

instance ParseRecord (Config Wrapped)

deriving instance Show (Config Unwrapped)

deriving instance FromDhall (Config Unwrapped)

deriving instance ToDhall (Config Unwrapped)

instance Default (Config Unwrapped) where
  def = fromJust $ unwrapRecordPure []

----------------------------------------
--- Generic Merging of record fields ---
----------------------------------------
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
