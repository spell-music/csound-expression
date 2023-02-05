-- | Csound types
module Csound.Typed.Core.Types
  ( module X
  , pureTuple
  , dirtyTuple
  -- * Rate conversions
  , K (..)
  , setRate, ar, kr, ir
  -- * control rate ref
  , sensorRef
  ) where

import Csound.Typed.Core.Types.Gen       as X
import Csound.Typed.Core.Types.Prim      as X
import Csound.Typed.Core.Types.Tuple     as X
import Csound.Typed.Core.Types.SE        as X
import Csound.Typed.Core.Types.SE.Logic  as X
import Csound.Typed.Core.Types.SE.Ref    as X
import Csound.Typed.Core.Types.Array     as X
import Csound.Typed.Core.Types.PureArray as X

import Csound.Dynamic (E, depT, MultiOut, toCtrlRate, Rate (..))
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.State (Run)
import Control.Monad.Trans.Class (lift)

---------------------------------------------------------------------------------------
-- tuple constructors

pureTuple :: forall a . Tuple a => Run (MultiOut [E]) -> a
pureTuple a =
  toTuple $ fmap ($ tupleArity @a) a

dirtyTuple :: forall a . Tuple a => Run (MultiOut [E]) -> SE a
dirtyTuple a =
  fmap (toTuple . return) $ SE
    $ mapM depT =<< (lift $ fmap ($ (tupleArity @a)) a)

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
setRate rate = liftE (Dynamic.setRate rate)

-- | An alias for the function @newRef@. It returns not the reference
-- to mutable value but a pair of reader and writer functions.
sensorRef :: Tuple a => a -> SE (SE a, a -> SE ())
sensorRef a = do
    ref <- newRef (K a)
    return $ (unK <$> readRef ref, writeRef ref . K)
