{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Hattier.Types where

import Control.Monad.RWS (RWS, execRWS)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Maybe (fromJust)
import Dhall (FromDhall, Natural)
import Dhall.Marshal.Encode (ToDhall)
import GHC.Generics
import GHC.Hs (GhcPs, HsModule)
import Options.Generic hiding (Text)

execHattier :: Hattier -> Config Unwrapped -> FormatterState -> (Text, Log)
execHattier hat cfg st =
  let (finalState, logs) = execRWS hat cfg st
   in (toLazyText (builder finalState), logs)

type Hattier = RWS (Config Unwrapped) Log FormatterState ()

data Config w = Config
  { indentWidth   :: w ::: Natural <#> "i" <!> "2"     <?> "The desired indentation width"
  , maxLineLength :: w ::: Natural <#> "m" <!> "130"   <?> "Number of characters before a forced line break"
  , defCfg        :: w ::: Bool    <#> "d" <!> "false" <?> "Print out the default configuration file"
  } deriving (Generic)

instance ParseRecord        (Config Wrapped)
deriving instance Show      (Config Unwrapped)
deriving instance FromDhall (Config Unwrapped)
deriving instance ToDhall   (Config Unwrapped)

defaultConfig :: Config Unwrapped
defaultConfig = fromJust $ unwrapRecordPure []

mergeConfigs :: [Config Unwrapped] -> Config Unwrapped
mergeConfigs [] = defaultConfig
mergeConfigs cs = foldr1 const cs

type Log = [Text]

data FormatterState = FormatterState
  { currentIndent :: !Int
  , currentColumn :: !Int
  , builder :: Builder -- The rendered source code so far
  }

initialState :: FormatterState
initialState =
  FormatterState {currentIndent = 0, currentColumn = 0, builder = mempty}

type HattierModule = HsModule GhcPs

----------------------------------------
--- Generic Merging of record fields ---
----------------------------------------

class MergeFlags a where
  mergeFlags :: a -> a -> a -> a
  default mergeFlags :: (Generic a, GMerge (Rep a)) =>  a -> a -> a -> a
  mergeFlags def base flags = to (gmerge (from def) (from base) (from flags))

class GMerge f where
  gmerge :: f p -> f p -> f p -> f p

instance GMerge f => GMerge (M1 i c f) where
  gmerge (M1 d) (M1 b) (M1 f) = M1 (gmerge d b f)

instance (GMerge a, GMerge b) => GMerge (a :*: b) where
  gmerge (d1 :*: d2) (b1 :*: b2) (f1 :*: f2) =
    gmerge d1 b1 f1 :*: gmerge d2 b2 f2

instance Eq a => GMerge (K1 i a) where
  gmerge (K1 def) (K1 base) (K1 flag)
    | flag == def = K1 base
    | otherwise   = K1 flag

instance MergeFlags (Config Unwrapped)
