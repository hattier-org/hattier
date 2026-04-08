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
import Hattier.Printer.Declaration.Value.Guard
import Hattier.Printer.Declaration.Value.Let
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
  indW <- asks (fromIntegral . indentWidth . cfg)
  append fname
  printPatsWithPadding (zip pats maxWidths)
  case grhss of
    GRHSs _ grhsList _ ->
      case grhsList of
        [L _ (GRHS _ [] body)] -> case unLoc body of
          HsLet _ binds letBody ->
            -- Break let to its own indented line so that 'in' can be correctly
            -- placed at column indW, matching the 'let' keyword column.
            append " =" >> newline >> append (T.replicate indW " ") >> printLetExpr binds letBody
          _ -> append " = " >> printExpr (unLoc body)
        _ ->
          -- Guarded RHS: each guard on its own indented line.
          mapM_ (\grhs -> newline >> printGuard (T.replicate indW " ") printExpr grhs) grhsList

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
