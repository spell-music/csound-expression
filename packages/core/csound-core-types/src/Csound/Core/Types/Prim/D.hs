-- | Init rate constants, they are expressed as
-- i-rate numbers in the rendered code
module Csound.Core.Types.Prim.D
  ( D (..), unD
  , D2, D3, D4, D5, D6
  ) where

import Csound.Dynamic (E, Rate (..))
import Csound.Dynamic qualified as Dynamic
import Csound.Core.State (Run)
import Csound.Core.Types.Prim.Val

-- | Constant numbers
data D
    = D  (Run E)
    | PrimD Double

unD :: D -> Run E
unD = toE

instance Val D where
    fromE  = D

    toE = \case
      D a     -> a
      PrimD d -> pure $ Dynamic.double d

    valRate = Ir

instance IsPrim D where
    type PrimOf D = Double

    getPrim x = case x of
        PrimD a -> Just a
        _         -> Nothing

    fromPrim = PrimD

instance Num D where
  (+) = liftPrim2 (+) (+)
  (*) = liftPrim2 (*) (*)
  negate = liftPrim negate negate
  abs = liftPrim abs abs
  signum = liftPrim signum signum
  fromInteger = PrimD . fromInteger

instance Fractional D where
  fromRational = PrimD . fromRational
  recip = liftPrim recip recip
  (/) = liftPrim2 (/) (/)

instance Floating D where
    { pi = PrimD pi;  exp = liftPrim exp exp;  sqrt = liftPrim sqrt sqrt; log = liftPrim log log;  logBase = liftPrim2 logBase logBase; (**) = liftPrim2 (**) (**)
    ; sin = liftPrim sin sin;  tan = liftPrim tan tan;  cos = liftPrim cos cos; sinh = liftPrim sinh sinh; tanh = liftPrim tanh tanh; cosh = liftPrim cosh cosh
    ; asin = liftPrim asin asin; atan = liftPrim atan atan;  acos = liftPrim acos acos ; asinh = liftPrim asinh asinh; acosh = liftPrim acosh acosh; atanh = liftPrim atanh atanh }

type D2 = (D, D)
type D3 = (D, D, D)
type D4 = (D, D, D, D)
type D5 = (D, D, D, D, D)
type D6 = (D, D, D, D, D, D)

