module Hattier.Printer.Combinators where

import Control.Monad.RWS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B
import Hattier.Types

append :: Text -> Hattier
append "" = pure ()
append txt = do
  s <- get
  let ind = currentIndent s
      col = currentColumn s
      spaces =
        if col == 0
          then T.replicate ind " "
          else mempty
      indentedTxt = spaces <> txt
  modify' $ \s' ->
    s'
      { builder = builder s <> B.fromText indentedTxt
      , currentColumn = currentColumn s + T.length indentedTxt
      }

newline :: Hattier
newline = do
  modify' $ \s -> s {builder = builder s <> "\n", currentColumn = 0}

incrIndent :: Hattier
incrIndent = do
  width <- asks indentWidth
  modify' $ \s -> s {currentIndent = currentIndent s + width}

decrIndent :: Hattier
decrIndent = do
  width <- asks indentWidth
  modify' $ \s -> s {currentIndent = max 0 (currentIndent s - width)}
