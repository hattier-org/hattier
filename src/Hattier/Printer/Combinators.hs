-- | Primitive combinators for building up formatted output.
--
-- All output is produced by composing these building blocks. Only 'append'
-- and 'newline' may modify 'currentColumn'; everything else in the printer
-- should produce output through them.
module Hattier.Printer.Combinators where

import Control.Monad (when)
import Control.Monad.RWS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import GHC.Utils.Outputable (Outputable)
import Hattier.Printer.Utils
import Hattier.Types

-- | Append a 'Text' fragment to the output, advancing 'currentColumn' by the
-- text's length. No-ops on the empty string. Besides 'newline', this is the
-- only function that should modify @currentColumn@.
append :: Text -> Hattier
append "" = pure ()
append txt = modify' $ \s ->
  s
    { builder = builder s <> B.fromText txt,
      currentColumn = currentColumn s + T.length txt
    }

-- | Emit a newline character and reset 'currentColumn' to zero.
newline :: Hattier
newline = modify' $ \s ->
  s
    { builder = builder s <> "\n",
      currentColumn = 0
    }

-- | Print each action with a separator between them but nothing after the last.
withSep :: Hattier -> [Hattier] -> Hattier
withSep _ [] = pure ()
withSep sep (x : xs) = x >> mapM_ (sep >>) xs

-- | Render any 'Outputable' GHC AST node via its 'ppr' instance and append
-- the result. Also records a warning in the 'Log' so fallback sites can be
-- identified and replaced with proper printers over time.
fallback :: (Outputable a) => a -> Hattier
fallback a = tell ["fallback: ", TL.show (pprText a), "\n"] >> (append . pprText) a

-- | Append an anchor for a given 'Hattier' context. This is the
-- only function that should change @currentAnchor@
withAnchor :: Int -> Hattier -> Hattier
withAnchor anch = local (\env -> env {currentAnchor = anch})

-- | Emit spaces until 'currentColumn' reaches @anchor@.
-- Does nothing if the cursor is already at or past @anchor@.
indentTo :: Int -> Hattier
indentTo anchor = do
  current <- gets currentColumn
  when (current < anchor) $
    append (T.replicate (anchor - current) " ")
