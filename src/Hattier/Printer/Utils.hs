-- | Utility functions for the Hattier printer.
--
-- Provides helpers for rendering GHC AST nodes to 'Text', computing alignment
-- widths, and measuring the printed width of patterns and bindings.
module Hattier.Printer.Utils where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import Hattier.Config
import Language.Haskell.Syntax.Basic

-- | Convert any GHC AST node to 'Text' via its 'Outputable' instance.
pprText :: (Outputable a) => a -> Text
pprText = T.pack . showSDocUnsafe . ppr

-- | Pad text on the right to at least @n@ characters.
padTo :: Int -> Text -> Text
padTo n t = t <> T.replicate (max 0 (n - T.length t)) " "

-- | Produce a 'Text' of @n@ space characters.
indToText :: Int -> Text
indToText n = T.replicate n " "

-- | Given an 'Alignment' style and a list of item widths, return the target
-- column width: the maximum width for 'PrimaryAlignment', or @0@ for 'NoAlignment'.
computeAlignment :: Alignment -> [Int] -> Int
computeAlignment PrimaryAlignment xs = maximum xs
computeAlignment NoAlignment _ = 0

-- | Like 'computeAlignment', but for a matrix of widths (one inner list per
-- column position). Returns the per-column target widths.
computeColumnAlignments :: Alignment -> [[Int]] -> [Int]
computeColumnAlignments PrimaryAlignment cols = map maximum cols
computeColumnAlignments NoAlignment cols = map (const 0) cols

-- | Compute the printed character width of a pattern. Used to determine the
-- padding needed to align patterns across function clauses or case alternatives.
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
    ConPat _ (L _ name) (RecCon _) -> T.length $ pprText name -- TODO: records
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

-- | Compute the printed width of the single pattern in a single-pattern 'Match'.
-- Returns @0@ for matches with zero or more than one pattern.
matchWidth :: LMatch GhcPs (LHsExpr GhcPs) -> Int
matchWidth (L _ Match {m_pats = [pat]}) = patWidth pat
matchWidth _ = 0

-- | Return the printed length of the name in a 'FunBind'. Used to compute
-- alignment widths for let-binding names. Returns @0@ for non-'FunBind' forms.
nameLen :: LHsBind GhcPs -> Int
nameLen (L _ (FunBind _ lname _)) = T.length (pprText $ unLoc lname)
nameLen _ = 0
