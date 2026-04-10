module Hattier.Printer.Declaration.Signature
  ( printSignature,
  )
where

import GHC.Hs
import Hattier.Printer.Combinators
import Hattier.Printer.Declaration.Signature.Type
import Hattier.Types

printSignature :: Sig GhcPs -> Hattier
printSignature (TypeSig _ names sigType) = printTypeSig names sigType
printSignature sig = fallback sig
