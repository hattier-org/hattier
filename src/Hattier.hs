module Hattier where

import Hattier.Format (fmt)
import Hattier.Types (Hattier, HattierModule)

hattier :: HattierModule -> Hattier
hattier = fmt
