-- | Printer for declaration signatures (type signatures and similar).
module Hattier.Printer.Declaration.Signature
  ( printSignature,
  )
where

import GHC.Hs
import Hattier.Printer.Combinators
import Hattier.Printer.Declaration.Signature.Type
import Hattier.Types

-- | Print a signature declaration. Handles 'TypeSig'; falls back to 'ppr'
-- for other signature forms.
printSignature :: Sig GhcPs -> Hattier
printSignature (TypeSig _ names sigType) = printTypeSig names sigType
printSignature sig = fallback sig
