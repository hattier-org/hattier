module Hattier.Printer.Combinators where

import Control.Monad.RWS
import Data.Text (Text)
import Data.Text.Lazy.Builder qualified as B
import GHC.Utils.Outputable (Outputable)
import Hattier.Printer.Utils
import Hattier.Types

append :: Text -> Hattier
append "" = pure ()
append txt = modify' $ \s -> s {builder = builder s <> B.fromText txt}

newline :: Hattier
newline = append "\n"

-- | Print each action with a separator between them but nothing after the last.
withSep :: Hattier -> [Hattier] -> Hattier
withSep _ [] = pure ()
withSep sep (x : xs) = x >> mapM_ (sep >>) xs

fallback :: (Outputable a) => a -> Hattier
fallback a = tell ["fallback"] >> (append . pprText) a
