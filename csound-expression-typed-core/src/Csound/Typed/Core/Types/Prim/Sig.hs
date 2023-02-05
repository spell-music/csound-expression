-- | Control or audio rate signals,
-- expressed as a-rate or k-rate signals in Csound code.
-- The type depends on the context and derived from the context.
-- Also type can by specified by the user with functions @ar@ and @kr@.
module Csound.Typed.Core.Types.Prim.Sig
  ( Sig (..)
  , unSig
  , Sig2, Sig3, Sig4, Sig5, Sig6, Sig7, Sig8
  ) where

import Csound.Dynamic (E, Rate (..))
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.State (Run)
import Csound.Typed.Core.Types.Prim.Val

-- | Signals
data Sig
    = Sig  (Run E)
    | PrimSig Double

instance Val Sig where
    fromE  = Sig

    toE = \case
      Sig a     -> a
      PrimSig d -> pure $ Dynamic.double d

    valRate = Ar

unSig :: Sig -> Run E
unSig = toE

instance IsPrim Sig where
    type PrimOf Sig = Double

    getPrim x = case x of
        PrimSig a -> Just a
        _         -> Nothing

    fromPrim = PrimSig

instance Num Sig where
  (+) = liftPrim2 (+) (+)
  (*) = liftPrim2 (*) (*)
  negate = liftPrim negate negate
  abs = liftPrim abs abs
  signum = liftPrim signum signum
  fromInteger = PrimSig . fromInteger

instance Fractional Sig where
  fromRational = PrimSig . fromRational
  recip = liftPrim recip recip
  (/) = liftPrim2 (/) (/)

instance Floating Sig where
    { pi = PrimSig pi;  exp = liftPrim exp exp;  sqrt = liftPrim sqrt sqrt; log = liftPrim log log; logBase = liftPrim2 logBase logBase; (**) = liftPrim2 (**) (**)
    ; sin = liftPrim sin sin;  tan = liftPrim tan tan;  cos = liftPrim cos cos; sinh = liftPrim sinh sinh; tanh = liftPrim tanh tanh; cosh = liftPrim cosh cosh
    ; asin = liftPrim asin asin; atan = liftPrim atan atan;  acos = liftPrim acos acos ; asinh = liftPrim asinh asinh; acosh = liftPrim acosh acosh; atanh = liftPrim atanh atanh }


type Sig2 = (Sig, Sig)
type Sig3 = (Sig, Sig, Sig)
type Sig4 = (Sig, Sig, Sig, Sig)
type Sig5 = (Sig, Sig, Sig, Sig, Sig)
type Sig6 = (Sig, Sig, Sig, Sig, Sig, Sig)
type Sig7 = (Sig, Sig, Sig, Sig, Sig, Sig, Sig)
type Sig8 = (Sig, Sig, Sig, Sig, Sig, Sig, Sig, Sig)

