module Main
  ( main,
  )
where

import Control.Applicative ((<|>))
import Control.Exception.Safe
import Control.Monad (when)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text.IO qualified as T (readFile)
import Data.Text.Lazy qualified as TL (concat)
import Data.Text.Lazy.IO qualified as TL (appendFile, putStrLn, writeFile)
import Data.Version (showVersion)
import Dhall (auto, inputFile)
import Hattier
import Options.Generic (Unwrapped, getWithHelp, unHelpful, unwrap)
import Paths_hattier qualified as Paths (version)
import System.Directory (XdgDirectory (..), getCurrentDirectory, getXdgDirectory)
import System.Exit (exitSuccess)
import Prelude hiding (log)

main :: IO ()
main = do
  --------------------------
  --- Read configuration ---
  --------------------------
  sysDhall <- pure Nothing -- TODO: implement
  usrDhall <- loadDhallIfExists =<< getXdgDirectory XdgConfig ""
  prjDhall <- loadDhallIfExists =<< getCurrentDirectory
  (CLI flags posArg, help) <- getWithHelp "<hattier3"
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

  -----------------------------------
  --- Read source files to format ---
  -----------------------------------
  let (PosArg mfile) = posArg
  file <- maybe (help >> exitSuccess) pure (unHelpful mfile)
  source <- T.readFile file
  ast' <- either throwIO pure $ parseTextToAST source defaultParserOpts
  let env = Env ast' config

  --------------
  --- Format ---
  --------------
  let (out, log) = execHattier hattier env initialState
  TL.appendFile "hattier.log" $ TL.concat log
  if inPlace config
    then TL.writeFile file out
    else TL.putStrLn out

-- |
--  We do not need to notify the user if a dhall config file is not present,
--  since we can always recover by applying default configuration options always.
loadDhallIfExists :: FilePath -> IO (Maybe (Config Unwrapped))
loadDhallIfExists fp =
  catchIO
    (Just <$> inputFile auto (fp <> "/hattier.dhall"))
    (const $ pure Nothing)
