-- | Spectrums
module Csound.Core.Types.Prim.Spec
  ( Spec (..)
  ) where

import Csound.Dynamic (E, Rate (..))
import Csound.Core.State (Run)
import Csound.Core.Types.Prim.Val

-- | Strings
newtype Spec = Spec { unSpec :: Run E }

instance Val Spec where
  fromE = Spec
  toE   = unSpec
  valRate = Fr
