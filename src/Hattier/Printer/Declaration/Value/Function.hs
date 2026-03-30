module Hattier.Printer.Declaration.Value.Function
  ( printFunBind,
  )
where

import Control.Monad.RWS
import Data.List (transpose)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.SrcLoc
import Hattier.Config
import Hattier.Printer.Combinators
import Hattier.Printer.Expression
import Hattier.Types

printFunBind ::
  RdrName ->
  [ GenLocated
      SrcSpanAnnA
      (Match GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)))
  ] ->
  Hattier
printFunBind name matches = do
  style <- asks (funcAlignment . cfg)
  let fname = pprText name
  case style of
    PrimaryAlignment -> do
      let clausePatterns = [pats | L _ Match {m_pats = pats} <- matches]
          maxWidths = map (maximum . map patWidth) (transpose clausePatterns)
      -- splitting up these cases enables us to only put
      -- newlines in between declarations and not after the
      -- final one.
      case matches of
        [] -> pure ()
        (x : xs) -> do
          printClause fname maxWidths x
          mapM_ (\clause -> newline >> printClause fname maxWidths clause) xs
    NoAlignment -> do
      -- See the comment at the above case expression
      case matches of
        [] -> pure ()
        (x : xs) -> do
          append $ pprText x
          mapM_ (\match -> newline >> (append $ pprText match)) xs

printClause ::
  Text ->
  [Int] ->
  GenLocated
    SrcSpanAnnA
    (Match GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) ->
  Hattier
printClause fname maxWidths (L _ Match {m_pats = pats, m_grhss = grhss}) = do
  append fname
  printPatsWithPadding (zip pats maxWidths)
  append " = "
  case grhss of
    GRHSs _ grhsList _ ->
      case grhsList of
        (L _ (GRHS _ [] body)) : _ -> printExpr (unLoc body)
        _ -> undefined -- TODO: more complex rhs: guarded, multi-rhs, etc.

-- This helper function makes sure we only add padding after a function
-- argument if it isn't the last argument
-- TODO: instead of simply printing the pattern with 'pprText', this
-- function should also take account of maximum widths to align patterns
-- within patterns like those inside tuples.
printPatsWithPadding :: [(LPat GhcPs, Int)] -> Hattier
printPatsWithPadding [] = pure ()
printPatsWithPadding ((pat, maxWidth) : rest) = do
  append " "
  append $ pprText pat
  append $ T.replicate (maxWidth - patWidth pat) " "
  printPatsWithPadding rest
