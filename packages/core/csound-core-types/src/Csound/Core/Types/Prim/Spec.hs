-- | Spectrums
module Csound.Core.Types.Prim.Spec (
  Spec (..),
) where

import Csound.Core.State (Run)
import Csound.Core.Types.Prim.Val
import Csound.Dynamic (E, Rate (..))

-- | Spectrum
newtype Spec = Spec {unSpec :: Run E}

instance Val Spec where
  fromE = Spec
  toE = unSpec
  valRate = Fr
