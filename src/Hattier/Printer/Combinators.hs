module Hattier.Printer.Combinators where

import Control.Monad.RWS
import Data.Text (Text)
import Data.Text.Lazy.Builder qualified as B
import GHC.Utils.Outputable (Outputable)
import Hattier.Config
import Hattier.Printer.Utils
import Hattier.Types

append :: Text -> Hattier ()
append "" = pure ()
append txt = modify' $ \s -> s {builder = builder s <> B.fromText txt}

newline :: Hattier ()
newline = append "\n"

-- | Print each action with a separator between them but nothing after the last.
withSep :: Hattier a -> [Hattier a] -> Hattier ()
withSep _ [] = pure ()
withSep sep (x : xs) = x >> mapM_ (sep >>) xs

fallback :: (Outputable a) => a -> Hattier ()
fallback a = tell ["fallback"] >> (append . pprText) a

-- | Increase the indentation level for a given 'Hattier' context
withIndent :: Hattier a -> Hattier a
withIndent = local (\env -> env {indentLevel = indentLevel env + 1})

askIndent :: Hattier Text
askIndent = do
  indW <- asks (fromIntegral . indentWidth . cfg)
  indL <- asks indentLevel
  pure $ indToText (indW * indL)

askIndentPlus :: Int -> Hattier Text
askIndentPlus n = do
  indW <- asks (fromIntegral . indentWidth . cfg)
  indL <- asks indentLevel
  pure $ indToText (indW * indL + n)
