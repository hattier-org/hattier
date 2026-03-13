module Main
  ( main
  ) where

import Control.Exception (throw)
import Hattier
import Hattier.Parser
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
