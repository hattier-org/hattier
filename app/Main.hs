module Main (main) where

import Hattier
import Hattier.Types

main :: IO ()
main = do
  --------------------------
  --- Read configuration ---
  --------------------------
  let config = defaultConfig

  -----------------------------------
  --- Read source files to format ---
  -----------------------------------
  let sourceFiles = undefined

  --------------
  --- Format ---
  --------------
  let _ = execHattier hattier config sourceFiles

  return ()
