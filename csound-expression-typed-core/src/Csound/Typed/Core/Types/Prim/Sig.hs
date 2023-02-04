-- | Control or audio rate signals,
-- expressed as a-rate or k-rate signals in Csound code.
-- The type depends on the context and derived from the context.
-- Also type can by specified by the user with functions @ar@ and @kr@.
module Csound.Typed.Core.Types.Prim.Sig
  ( Sig (..)
  , Sig3, Sig2, Sig4, Sig5, Sig6, Sig7, Sig8
  ) where

import Csound.Dynamic (E)
import Csound.Typed.Core.State (Run)
import Csound.Typed.Core.Types.Prim.Val

newtype Sig = Sig { unSig :: Run E }

instance Val Sig where
  fromE = Sig
  toE   = unSig

instance Num Sig where
  (+) = liftE2 (+)
  (*) = liftE2 (*)
  negate = liftE negate
  abs = liftE abs
  signum = liftE signum
  fromInteger = fromE . pure . fromInteger

instance Fractional Sig where
  fromRational = Sig . pure . fromRational
  recip = liftE recip
  (/) = liftE2 (/)

type Sig2 = (Sig, Sig)
type Sig3 = (Sig, Sig, Sig)
type Sig4 = (Sig, Sig, Sig, Sig)
type Sig5 = (Sig, Sig, Sig, Sig, Sig)
type Sig6 = (Sig, Sig, Sig, Sig, Sig, Sig)
type Sig7 = (Sig, Sig, Sig, Sig, Sig, Sig, Sig)
type Sig8 = (Sig, Sig, Sig, Sig, Sig, Sig, Sig, Sig)
