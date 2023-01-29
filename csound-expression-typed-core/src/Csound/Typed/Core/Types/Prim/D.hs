module Csound.Typed.Core.Types.Prim.D
  ( D (..)
  ) where

import Csound.Dynamic (E)
import Csound.Typed.Core.State (Run)
import Csound.Typed.Core.Types.Prim.Val

newtype D = D { unD :: Run E }

instance Val D where
  fromE = D
  toE   = unD

instance Num D where
  (+) = liftE2 (+)
  (*) = liftE2 (*)
  negate = liftE negate
  abs = liftE abs
  signum = liftE signum
  fromInteger = fromE . pure . fromInteger

instance Fractional D where
  fromRational = fromE . pure . fromRational
  recip = liftE recip
  (/) = liftE2 (/)
