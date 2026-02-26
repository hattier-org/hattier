module Main (main) where

import Format
import Types

main :: IO ()
main = do
  --------------------------
  --- Read configuration ---
  --------------------------
  let config = Config 2 80

  -----------------------------------
  --- Read source files to format ---
  -----------------------------------
  let sourceFiles = undefined

  --------------
  --- Format ---
  --------------
  let _ = execHattier fmt config sourceFiles

  return ()
