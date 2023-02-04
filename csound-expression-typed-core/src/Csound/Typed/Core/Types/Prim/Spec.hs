-- | Spectrums
module Csound.Typed.Core.Types.Prim.Spec
  ( Spec (..)
  ) where

import Csound.Dynamic (E)
import Csound.Typed.Core.State (Run)
import Csound.Typed.Core.Types.Prim.Val

-- | Strings
newtype Spec = Spec { unSpec :: Run E }

instance Val Spec where
  fromE = Spec
  toE   = unSpec
