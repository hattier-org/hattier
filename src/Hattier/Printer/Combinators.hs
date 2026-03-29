module Hattier.Printer.Combinators where

import Control.Monad.RWS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.Builder qualified as B
import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import Hattier.Types
import Language.Haskell.Syntax.Basic

append :: Text -> Hattier
append "" = pure ()
append txt = modify' $ \s -> s {builder = builder s <> B.fromText txt}

newline :: Hattier
newline = append "\n"

-- | Convert any GHC AST node to 'Text' via its 'Outputable' instance.
pprText :: (Outputable a) => a -> Text
pprText = T.pack . showSDocUnsafe . ppr

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
