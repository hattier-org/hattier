module Integration.Helpers
  ( runFullFormatter,
    runFullFormatterWith,
  )
where

import Data.Default (def)
import Data.Text qualified as T
import Data.Text.Lazy qualified as T.Lazy
import Hattier (hattier)
import Hattier.Config
import Hattier.Parser
import Hattier.Types
import Options.Generic (Unwrapped)

runFullFormatter :: T.Text -> T.Text
runFullFormatter = runFullFormatterWith def

runFullFormatterWith :: Config Unwrapped -> T.Text -> T.Text
runFullFormatterWith config src =
  let ast' = case parseTextToAST src defaultParserOpts of
        Right m -> m
        Left err -> error $ "parse error: " <> show err
      env = Env ast' config 1
   in T.Lazy.toStrict $ fst $ execHattier hattier env initialState
