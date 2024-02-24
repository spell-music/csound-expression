-- | Rate conversions
module Csound.Core.Types.Rate
  ( K (..)
  , setRate
  , ar, kr, ir
  ) where

import Csound.Dynamic (toCtrlRate, Rate (..))
import Csound.Dynamic qualified as Dynamic
import Csound.Core.Types.Prim.Val
import Csound.Core.Types.Tuple
import Csound.Dynamic.Render.Pretty(ppE)
import Debug.Trace

-- | Control rate signals or constants
-- it can be used to create references or channels with control-rate.
-- For example this will create a K-rate signal instead of Audio rate (by default):
--
-- > ref <- newRef (0 :: K Sig)
newtype K a = K { unK :: a }
  deriving (IsPrim, Val, Num, Fractional, Floating)

instance Tuple a => Tuple (K a) where
  tupleMethods = TupleMethods
    { tupleRates_ = fmap toCtrlRate (tupleRates @a)
    , defTuple_ = K (defTuple @a)
    , fromTuple_ = fromTuple . unK
    , toTuple_ = K . toTuple
    , tupleArity_ = tupleArity @a
    }

ar :: Val a => a -> a
ar = setRate Ar

kr :: Val a => a -> a
kr = setRate Kr

ir :: Val a => a -> a
ir = setRate Ir

setRate :: Val a => Rate -> a -> a
setRate rate = liftE (\expr -> trace (show $ unlines ["SET-RATE", show $ ppE expr]) $ Dynamic.setRate rate expr)
