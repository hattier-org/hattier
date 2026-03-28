module Main
  ( main,
  )
where

import Control.Applicative ((<|>))
import Control.Exception.Safe
import Control.Monad (when)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text.IO qualified as T (getContents, readFile)
import Data.Text.Lazy qualified as TL (concat)
import Data.Text.Lazy.IO qualified as TL (appendFile, putStrLn, writeFile)
import Data.Version (showVersion)
import Dhall (auto, inputFile)
import Hattier
import Options.Generic (Unwrapped, getRecord, unHelpful, unwrap)
import Paths_hattier qualified as Paths (version)
import System.Directory (XdgDirectory (..), getCurrentDirectory, getXdgDirectory)
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import Prelude hiding (log)

main :: IO ()
main = do
  --------------------------
  --- Read configuration ---
  --------------------------
  sysDhall <- pure Nothing -- TODO: implement
  usrDhall <- loadDhallIfExists =<< getXdgDirectory XdgConfig ""
  prjDhall <- loadDhallIfExists =<< getCurrentDirectory
  CLI flags posArg <- getRecord "<hattier3"
  let dhall = fromMaybe def (prjDhall <|> usrDhall <|> sysDhall)
  let config = mergeFlags def dhall (unwrap flags)

  --------------------------------------
  --- Non formatting related options ---
  --------------------------------------
  when (defaultConfig config) $ do
    print (def :: Config Unwrapped)
    exitSuccess

  when (version config) $ do
    putStrLn $ showVersion Paths.version
    exitSuccess

  ----------------------------
  --- Read input to format ---
  ----------------------------
  let (PosArg mfile) = posArg

  (source, writeOutput) <- case unHelpful mfile of
    Just file -> do
      src <- T.readFile file
      pure (src, if inPlace config then TL.writeFile file else TL.putStrLn)
    Nothing -> do
      src <- T.getContents
      pure (src, TL.putStrLn)

  ast' <- either throwIO pure $ parseTextToAST source defaultParserOpts
  let env = Env ast' config

  --------------
  --- Format ---
  --------------
  let (out, log) = execHattier hattier env initialState
  TL.appendFile "hattier.log" $ TL.concat log
  writeOutput out

-- |
--  We do not need to notify the user if a dhall config file is not present,
--  since we can always recover by applying default configuration options always.
loadDhallIfExists :: FilePath -> IO (Maybe (Config Unwrapped))
loadDhallIfExists fp =
  catchIO
    (Just <$> inputFile auto (fp </> "hattier.dhall"))
    (const $ pure Nothing)
