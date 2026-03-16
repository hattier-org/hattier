module Hattier.Printer.Combinators where

import Control.Monad.RWS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import Hattier.Types

append :: Text -> Hattier
append "" = pure ()
append txt = do
  modify' $ \s -> s {builder = builder s <> B.fromText txt}

newline :: Hattier
newline = append "\n"

-- | Convert any GHC AST node to 'Text' via its 'Outputable' instance.
pprText :: Outputable a => a -> Text
pprText = T.pack . showSDocUnsafe . ppr
