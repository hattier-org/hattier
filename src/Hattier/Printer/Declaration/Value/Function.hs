module Hattier.Printer.Declaration.Value.Function
  ( printFunBind
  ) where

import Control.Monad.RWS
import Data.List (transpose)
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Hs
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.SrcLoc
import Hattier.Config
import Hattier.Printer.Combinators
import Hattier.Types
import Language.Haskell.Syntax.Basic

printFunBind ::
     RdrName
  -> [GenLocated
        SrcSpanAnnA
        (Match GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)))]
  -> Hattier
printFunBind name matches = do
  style <- asks (letAlignment . cfg)
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
        (x:xs) -> do
          printClause fname maxWidths x
          mapM_ (\clause -> newline >> printClause fname maxWidths clause) xs
    NoAlignment -> do
      -- See the comment at the above case expression
      case matches of
        [] -> pure ()
        (x:xs) -> do
          append $ pprText x
          mapM_ (\match -> newline >> (append $ pprText match)) xs

printClause ::
     Text
  -> [Int]
  -> GenLocated
       SrcSpanAnnA
       (Match GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)))
  -> Hattier
printClause fname maxWidths (L _ Match {m_pats = pats, m_grhss = grhss}) = do
  append fname
  printPatsWithPadding (zip pats maxWidths)
  append " = "
  case grhss of
    GRHSs _ grhsList _ ->
      case grhsList of
        (L _ (GRHS _ [] body)):_ -> append $ pprText (unLoc body) -- simple rhs
        _ -> undefined -- TODO: more complex rhs: guarded, multi-rhs, etc.

-- This helper function makes sure we only add padding after a function
-- argument if it isn't the last argument
-- TODO: instead of simply printing the pattern with 'pprText', this
-- function should also take account of maximum widths to align patterns
-- within patterns like those inside tuples.
printPatsWithPadding :: [(LPat GhcPs, Int)] -> Hattier
printPatsWithPadding [] = pure ()
printPatsWithPadding ((pat, maxWidth):xs) = do
  append " "
  append $ pprText pat
  let padding = T.replicate (maxWidth - patWidth pat) " "
  append padding
  mapM_ (\pat' -> (append padding) >> (append $ pprText pat')) xs

patWidth :: LPat GhcPs -> Int
patWidth (L _ pat) =
  case pat of
    -- In most cases where a specific integer is added to the width, the
    -- comment after the case shows what that integer represents
    WildPat _ -> 1
    VarPat _ (L _ name) -> T.length (pprText name)
    LazyPat _ pat' -> 1 + patWidth pat' -- "~"
    AsPat _ (L _ name) pat' -> T.length (pprText name) + 1 + patWidth pat' -- "@"
    ParPat _ pat' -> 2 + patWidth pat' -- "(" + ")"
    BangPat _ pat' -> 1 + patWidth pat' -- "!"
    ListPat _ pats -> 2 + sum (map patWidth pats) + max 0 (length pats - 1) * 2 -- "[" + "]" + commas: ", "
    TuplePat _ pats Boxed ->
      2 + sum (map patWidth pats) + max 0 (length pats - 1) * 2 -- "(" + ")" + commas: ", "
    TuplePat _ pats Unboxed ->
      4 + sum (map patWidth pats) + max 0 (length pats - 1) * 2 -- "(#" + "#)" + commas: ", "
    ConPat _ (L _ name) (InfixCon l r) ->
      patWidth l + 1 + T.length (pprText name) + 1 + patWidth r -- " con "
    ConPat _ (L _ name) (PrefixCon _ args) ->
      T.length (pprText name) + sum (map (\a -> 1 + patWidth a) args)
    ConPat _ (L _ name) (RecCon _) -> T.length $ pprText name -- TODO: recursion
    LitPat _ lit -> T.length $ pprText lit
    NPat _ (L _ lit) (Just _) _ -> 1 + (T.length $ pprText lit) -- "-"
    NPat _ (L _ lit) Nothing _ -> T.length $ pprText lit
    SigPat _ pat' sig -> 2 + patWidth pat' + 4 + (T.length $ pprText sig) -- "(" + " :: " + ")"
    -- TODO: the following require certain language extensions
    SumPat _ _ _ _ -> undefined -- UnboxedSums extension
    ViewPat _ _ _ -> undefined -- ViewPatterns extension
    SplicePat _ _ -> undefined -- TemplateHaskell extension
    NPlusKPat _ _ _ _ _ _ -> undefined -- NPlusKPatterns extension
    EmbTyPat _ _ -> undefined -- ExplicitNamespaces and RequiredTypeArguments extensions
    InvisPat _ _ -> undefined -- TypeAbstractions extension
