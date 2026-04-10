-- | Top-level entry point for the Hattier Haskell source formatter.
--
-- Re-exports 'Hattier.Config', 'Hattier.Format', 'Hattier.Parser', and
-- 'Hattier.Types', providing everything needed to parse and format Haskell
-- source. 'hattier' is the canonical formatting action for library consumers.
module Hattier
  ( module Hattier.Config,
    module Hattier.Format,
    module Hattier.Parser,
    module Hattier.Types,
    hattier,
  )
where

import Hattier.Config
import Hattier.Format
import Hattier.Parser
import Hattier.Types

-- | The canonical formatting action. Equivalent to 'fmt'; exposed at the
-- top level as the primary entry point for consumers of this library.
hattier :: Hattier
hattier = fmt
