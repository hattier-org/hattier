module Hattier.Printer.Pattern where

import GHC.Hs
import Hattier.Printer.Combinators
import Hattier.Printer.Utils
import Hattier.Types

-- The second tuple element represents the padding that should be
-- added after the first tuple element. With NoAlignment, this
-- should be 0.
-- TODO: instead of simply printing the pattern with 'pprText', this
-- function should also take account of maximum widths to align patterns
-- within patterns like those inside tuples.
printPats :: [(LPat GhcPs, Int)] -> Hattier
printPats = mapM_ $ \(pat, maxWidth) -> do
  let patTxt = pprText pat
  append " " >> append (padTo maxWidth patTxt)
