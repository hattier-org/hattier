module Hattier.Printer.Combinators where

import Control.Monad.RWS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.Builder qualified as B
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import Hattier.Types

append :: Text -> Hattier
append "" = pure ()
append txt = modify' $ \s -> s {builder = builder s <> B.fromText txt}

newline :: Hattier
newline = append "\n"

-- | Convert any GHC AST node to 'Text' via its 'Outputable' instance.
pprText :: (Outputable a) => a -> Text
pprText = T.pack . showSDocUnsafe . ppr
