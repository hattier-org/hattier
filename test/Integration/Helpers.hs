module Integration.Helpers
  ( runFullFormatter,
  )
where

import Data.Default (def)
import Data.Text qualified as T
import Data.Text.Lazy qualified as T.Lazy
import Hattier (hattier)
import Hattier.Parser
import Hattier.Types

runFullFormatter :: T.Text -> T.Text
runFullFormatter src =
  let ast' = case parseTextToAST src defaultParserOpts of
        Right m -> m
        Left err -> error $ "parse error: " <> show err
      env = Env ast' def
   in T.Lazy.toStrict $ fst $ execHattier hattier env initialState