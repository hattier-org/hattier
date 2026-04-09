module Hattier.Printer.Expression (printExpr) where

import Control.Monad.RWS
import Data.Text (Text)
import Data.Text qualified as T
import Data.List (transpose)
import GHC.Hs
import GHC.Types.SrcLoc
import Hattier.Config
import Hattier.Printer.Combinators
import Hattier.Types

data FlatPat = FlatCon Text [Text]
             | FlatOther Text

printExpr :: HsExpr GhcPs -> Hattier
printExpr (HsCase _ scrut mg) = printCaseExpr scrut mg
printExpr expr = append (pprText expr)

printCaseExpr :: LHsExpr GhcPs -> MatchGroup GhcPs (LHsExpr GhcPs) -> Hattier
printCaseExpr scrut (MG _ (L _ matches)) = do
  append "case "
  printExpr (unLoc scrut)
  append " of"
  newline

  indW <- asks (fromIntegral . indentWidth . cfg)
  style <- asks (caseAlignment . cfg)
  let ind = T.replicate indW " "

  case matches of
    [] -> pure ()
    (x : xs) -> case style of
      PrimaryAlignment -> do
        let flats = map flattenMatch matches
            cols = collectCols flats
            widths = maxWidths cols
        printAltAlign ind widths x
        mapM_ (\m -> newline >> printAltAlign ind widths m) xs
      NoAlignment -> do
        printAltNoAlign ind x
        mapM_ (\m -> newline >> printAltNoAlign ind m) xs

printAltAlign :: Text -> [Int] -> LMatch GhcPs (LHsExpr GhcPs) -> Hattier
printAltAlign ind widths alt@(L _ Match {m_pats = pats, m_grhss = grhss}) = do
  case pats of
    [] -> fallback
    _ -> do
      let flat = flattenMatch alt
          patTxt = renderFlat widths flat

      append ind
      append patTxt
      append " -> "

      case grhss of
        GRHSs _ [L _ (GRHS _ [] body)] _ ->
          printExpr (unLoc body)
        _ ->
          fallback
  where
    fallback = do
      tell ["printAltAlign: fallback"]
      append ind
      append (pprText alt)

printAltNoAlign :: Text -> LMatch GhcPs (LHsExpr GhcPs) -> Hattier
printAltNoAlign ind alt@(L _ Match {m_pats = pats, m_grhss = grhss}) = do
  case pats of
    [] -> fallback
    _ -> do
      append ind
      append (T.intercalate " " (map pprText pats))
      append " -> "

      case grhss of
        GRHSs _ [L _ (GRHS _ [] body)] _ ->
          printExpr (unLoc body)
        _ ->
          fallback
  where
    fallback = do
      tell ["printAltNoAlign: fallback"]
      append ind
      append (pprText alt)

flattenMatch :: LMatch GhcPs (LHsExpr GhcPs) -> FlatPat
flattenMatch (L _ Match {m_pats = [pat]}) = flattenPat pat
flattenMatch m = FlatOther (pprText m)

renderFlat :: [Int] -> FlatPat -> Text
renderFlat widths (FlatCon name args) =
  let cols = name : args
      padded = zipWith (\txt w -> txt <> T.replicate (w - T.length txt) " ") cols widths
   in T.intercalate " " padded
renderFlat _ (FlatOther txt) = txt

maxWidths :: [[Text]] -> [Int]
maxWidths cols = map (maximum . map T.length) (transpose cols)

flattenPat :: LPat GhcPs -> FlatPat
flattenPat (L _ pat) =
  case pat of
    ConPat _ (L _ name) (PrefixCon _ args) ->
      FlatCon
        (pprText name)
        (map (pprText . unLoc) args)
    _ ->
      FlatOther (pprText pat)

collectCols :: [FlatPat] -> [[Text]]
collectCols pats = [ name : args
                   | FlatCon name args <- pats ]
