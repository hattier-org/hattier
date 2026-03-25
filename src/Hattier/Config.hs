{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Hattier.Config where

import Data.Maybe (fromJust)
import Dhall (FromDhall, Natural)
import Dhall.Marshal.Encode (ToDhall)
import GHC.Generics
import Options.Generic
import Data.Default (Default, def)

data CLI =  CLI (Config Wrapped) PosArg deriving (Generic)

newtype PosArg = PosArg (Maybe FilePath <?> "file name") deriving (Generic)

data Config w = Config
  { indentWidth   :: w ::: Natural     <#> "w"   <!> "2"               <?> "The desired indentation width"
  , letAlignment  :: w ::: LetAlignment<#> "let" <!> "PrimaryAlignment" <?> "The alignment style for let-bindings: one-line, no-alignment, or primary-alignment"
  , inPlace       :: w ::: Bool        <#> "i"   <!> "false"           <?> "edit files in place"
  , version       :: w ::: Bool        <#> "v"   <!> "false"           <?> "hattier version"
  , defaultConfig :: w ::: Bool        <#> "d"   <!> "false"           <?> "Print out the default configuration file"
  } deriving (Generic)


data LetAlignment
  = NoAlignment      -- ^ Indent each binding uniformly, no padding
  | PrimaryAlignment -- ^ Pad names so '=' signs align to the longest name
  | OneLine          -- ^ Collapse all bindings onto one line: let x = 1; y = 2 in body
  deriving (Eq, Generic, Read, Show, FromDhall, ToDhall)

instance ParseField  LetAlignment
instance ParseFields LetAlignment
instance ParseRecord LetAlignment

instance ParseRecord CLI where
  parseRecord = CLI <$> parseRecord <*> parseRecord

instance ParseRecord        PosArg

instance ParseRecord        (Config Wrapped)
deriving instance Show      (Config Unwrapped)
deriving instance FromDhall (Config Unwrapped)
deriving instance ToDhall   (Config Unwrapped)
instance Default            (Config Unwrapped) where
  def = fromJust $ unwrapRecordPure []

----------------------------------------
--- Generic Merging of record fields ---
----------------------------------------
class MergeFlags a where
  mergeFlags :: a -> a -> a -> a
  default mergeFlags :: (Generic a, GMerge (Rep a)) =>  a -> a -> a -> a
  mergeFlags dflt base flags = to (gmerge (from dflt) (from base) (from flags))

class GMerge f where
  gmerge :: f p -> f p -> f p -> f p

instance GMerge f => GMerge (M1 i c f) where
  gmerge (M1 d) (M1 b) (M1 f) = M1 (gmerge d b f)

instance (GMerge a, GMerge b) => GMerge (a :*: b) where
  gmerge (d1 :*: d2) (b1 :*: b2) (f1 :*: f2) =
    gmerge d1 b1 f1 :*: gmerge d2 b2 f2

instance Eq a => GMerge (K1 i a) where
  gmerge (K1 dflt) (K1 base) (K1 flag)
    | flag == dflt = K1 base
    | otherwise    = K1 flag

instance MergeFlags (Config Unwrapped)
