module Hattier.Printer.Combinators where

import Control.Monad (when)
import Control.Monad.RWS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import GHC.Utils.Outputable (Outputable)
import Hattier.Config
import Hattier.Printer.Utils
import Hattier.Types

-- besides 'newline', this is the only function that
-- should modify @currentColumn@.
append :: Text -> Hattier ()
append "" = pure ()
append txt = modify' $ \s ->
  s
    { builder = builder s <> B.fromText txt,
      currentColumn = currentColumn s + T.length txt
    }

newline :: Hattier ()
newline = modify' $ \s ->
  s
    { builder = builder s <> "\n",
      currentColumn = 0
    }

-- | Print each action with a separator between them but nothing after the last.
withSep :: Hattier a -> [Hattier a] -> Hattier ()
withSep _ [] = pure ()
withSep sep (x : xs) = x >> mapM_ (sep >>) xs

fallback :: (Outputable a) => a -> Hattier ()
fallback a = tell ["fallback: ", TL.show (pprText a), "\n"] >> (append . pprText) a

-- | Append an anchor for a given 'Hattier' context. This is the
-- only function that should change @currentAnchor@
withAnchor :: Int -> Hattier a -> Hattier a
withAnchor anch = local (\env -> env {currentAnchor = anch})

indentTo :: Int -> Hattier ()
indentTo anchor = do
  current <- gets currentColumn
  when (current < anchor) $
    append (T.replicate (anchor - current) " ")
