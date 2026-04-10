module Hattier.Printer.Declaration.Value.Function
  ( printFunBind,
  )
where

import Control.Monad.RWS
import Data.List (transpose)
import Data.Text (Text)
import GHC.Hs
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.SrcLoc
import Hattier.Config
import Hattier.Printer.Combinators
import Hattier.Printer.Expression
import Hattier.Printer.Pattern
import Hattier.Printer.Utils
import Hattier.Types

printFunBind ::
  RdrName ->
  [ GenLocated
      SrcSpanAnnA
      (Match GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)))
  ] ->
  Hattier ()
printFunBind name matches = do
  style <- asks (funcAlignment . cfg)
  let fname = pprText name
      clausePatterns = [pats | L _ Match {m_pats = pats} <- matches]
      maxWidths = computeColumnAlignments style (map (map patWidth) (transpose clausePatterns))

  withSep newline $ map (printClause fname maxWidths) matches

printClause ::
  Text ->
  [Int] ->
  GenLocated
    SrcSpanAnnA
    (Match GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) ->
  Hattier ()
printClause fname maxWidths (L _ Match {m_pats = pats, m_grhss = grhss}) = do
  ind <- askIndent
  append fname
  printPats (zip pats maxWidths)
  printRHS ind grhss
