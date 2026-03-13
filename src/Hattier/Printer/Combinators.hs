module Hattier.Printer.Combinators where

import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as B
import Hattier.Types

append :: Text -> Hattier
append txt = do
  col <- gets currentColumn
  modify'
    (\s ->
       s
         { builder = builder s <> B.fromText txt
         , currentColumn = currentColumn s + col
         })

newline :: Hattier
newline = append "\n"
