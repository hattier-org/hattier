module Hattier where

import Hattier.Format (fmt)
import Hattier.Types (HattierMonad)

hattier :: HattierMonad
hattier = fmt
