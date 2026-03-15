module Hattier.Printer.Combinators where

import Control.Monad.RWS
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as B
import Hattier.Types

append :: Text -> Hattier
append "" = pure ()
append txt = do
  modify' $ \s -> s {builder = builder s <> B.fromText txt}

newline :: Hattier
newline = append "\n"
