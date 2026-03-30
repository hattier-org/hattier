module Hattier.Printer.Expression (printExpr) where

import Control.Monad.RWS
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs
import GHC.Types.SrcLoc
import Hattier.Config
import Hattier.Printer.Combinators
import Hattier.Types

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
        let widths = map matchPatWidth matches
            maxWidth = maximum widths
        printAltAlign ind maxWidth x
        mapM_ (\m -> newline >> printAltAlign ind maxWidth m) xs
      NoAlignment -> do
        printAltNoAlign ind x
        mapM_ (\m -> newline >> printAltNoAlign ind m) xs

printAltAlign :: Text -> Int -> LMatch GhcPs (LHsExpr GhcPs) -> Hattier
printAltAlign ind maxWidth alt@(L _ Match {m_pats = pats, m_grhss = grhss}) = do
  case pats of
    [] -> fallback
    _ -> do
      let patTxt = T.intercalate " " (map pprText pats)
          padding = T.replicate (maxWidth - T.length patTxt) " "

      append ind
      append patTxt
      append padding
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

matchPatWidth :: LMatch GhcPs (LHsExpr GhcPs) -> Int
matchPatWidth (L _ Match {m_pats = [pat]}) = patWidth pat
matchPatWidth _ = 0
