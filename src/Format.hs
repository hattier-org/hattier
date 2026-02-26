module Format where

import Types
import Control.Monad.Reader (ask)
import Control.Monad.State  (modify)
import Data.Text qualified as T
import Data.Functor         ((<&>))

fmt :: HattierMonad
fmt = sequence_ [fmtExample {- add your formatters here-} ]

-- | Only for example purposes; please delete once proper formatters are written; moreover its wrong
fmtExample :: HattierMonad
fmtExample = do
  _indentWidth <- fromEnum . indentWidth <$> ask
  modify $ \sourceFiles ->
    sourceFiles <&> \sourceFile ->
      T.unlines $ T.lines sourceFile <&> \line ->
        T.replicate _indentWidth " " `T.append` line
