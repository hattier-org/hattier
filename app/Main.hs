module Main
  ( main
  ) where

import Control.Exception (throw)
import Control.Monad (guard, when)
import Data.Maybe (catMaybes)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Cont (evalContT, callCC)
import Dhall (inputFile, auto)
import Options.Generic (Unwrapped, unwrapRecord)
import System.Directory (XdgDirectory(..), getXdgDirectory, getCurrentDirectory, doesFileExist)

import Hattier
import Hattier.Parser
import Hattier.Types

main :: IO ()
main = evalContT $ callCC $ \earlyReturn -> do
  --------------------------
  --- Read configuration ---
  --------------------------
  sysDhall <- pure Nothing
  usrDhall <- liftIO $ loadDhallIfExists =<< getXdgDirectory XdgConfig ""
  prjDhall <- liftIO $ loadDhallIfExists =<< getCurrentDirectory
  flags    <- unwrapRecord "<hattier3"
  let dhall = mergeConfigs $ catMaybes [prjDhall, usrDhall, sysDhall]
  let config = mergeFlags defaultConfig dhall flags

  --------------------------------------
  --- Non formatting related options ---
  --------------------------------------
  when (defCfg config) $ do
    liftIO $ print defaultConfig
    earlyReturn ()

  -----------------------------------
  --- Read source files to format ---
  -----------------------------------
  let sourceFile = undefined
  let ast =
        case parseTextToAST sourceFile defaultParserOpts of
          Left e -> throw e
          Right ast' -> ast'
  --------------
  --- Format ---
  --------------
  let _ = execHattier (hattier ast) config initialState
  return ()

loadDhallIfExists :: FilePath -> IO (Maybe (Config Unwrapped))
loadDhallIfExists fp = runMaybeT $ do
  let fp' = fp <> "/hattier.dhall"
  guard =<< liftIO (doesFileExist fp')
  liftIO $ inputFile auto fp'
