module Main (main) where

import Format
import Types
import Dhall (input, auto)
import Options.Generic

main :: IO ()
main = do
  --------------------------
  --- Read configuration ---
  --------------------------
  flags <- unwrapRecord "<hattier3"
  dhall <- input auto "./config.dhall"

  putStrLn "Flags config:" >> print (flags :: Config Unwrapped)
  putStrLn "Dhall config:" >> print (dhall :: Config Unwrapped)

  let config = undefined

  -----------------------------------
  --- Read source files to format ---
  -----------------------------------
  let sourceFiles = undefined

  --------------
  --- Format ---
  --------------
  let _ = execHattier fmt config sourceFiles

  return ()
