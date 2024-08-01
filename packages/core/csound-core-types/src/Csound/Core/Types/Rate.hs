-- | Rate conversions
module Csound.Core.Types.Rate (
  K (..),
  setRate,
  ar,
  kr,
  ir,
  Rate (..),
) where

import Csound.Core.Types.Prim.Val
import Csound.Core.Types.Tuple
import Csound.Dynamic (Rate (..), toCtrlRate)
import Csound.Dynamic qualified as Dynamic

{- | Control rate signals or constants
it can be used to create references or channels with control-rate.
For example this will create a K-rate signal instead of Audio rate (by default):

> ref <- newRef (0 :: K Sig)
-}
newtype K a = K {unK :: a}
  deriving newtype (IsPrim, Val, Num, Fractional, Floating, FromTuple)

instance (Tuple a) => Tuple (K a) where
  tupleArity = tupleArity @a
  tupleRates = fmap toCtrlRate (tupleRates @a)
  defTuple = K defTuple
  toTuple = K . toTuple

ar :: (Val a) => a -> a
ar = setRate Ar

kr :: (Val a) => a -> a
kr = setRate Kr

ir :: (Val a) => a -> a
ir = setRate Ir

setRate :: (Val a) => Rate -> a -> a
setRate rate = liftE (\expr -> Dynamic.setRate rate expr)
