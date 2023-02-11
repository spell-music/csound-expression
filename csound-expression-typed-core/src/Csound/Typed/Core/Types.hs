-- | Csound types
module Csound.Typed.Core.Types
  ( module X
  , pureTuple
  , dirtyTuple
  -- * control rate ref
  , sensorRef
  ) where

import Csound.Typed.Core.Types.Gen       as X
import Csound.Typed.Core.Types.Prim      as X
import Csound.Typed.Core.Types.Tuple     as X
import Csound.Typed.Core.Types.SE        as X
import Csound.Typed.Core.Types.SE.Logic  as X
import Csound.Typed.Core.Types.SE.Ref    as X
import Csound.Typed.Core.Types.SE.Port   as X
import Csound.Typed.Core.Types.SE.Instr  as X
import Csound.Typed.Core.Types.Array     as X
import Csound.Typed.Core.Types.PureArray as X
import Csound.Typed.Core.Types.Rate      as X

import Csound.Dynamic (E, MultiOut)
import Csound.Typed.Core.State (Run)
-- import Control.Monad.Trans.Class (lift)

---------------------------------------------------------------------------------------
-- tuple constructors

pureTuple :: forall a . Tuple a => Run (MultiOut [E]) -> a
pureTuple a =
  toTuple $ fmap ($ tupleArity @a) a


dirtyTuple :: forall a . Tuple a => Run (MultiOut [E]) -> SE a
dirtyTuple _a = error "dirtyTuple: define me"
{-
  fmap (toTuple . return) $ SE
    $ mapM depT =<< (lift $ fmap ($ (tupleArity @a)) a)
-}

-- | An alias for the function @newRef@. It returns not the reference
-- to mutable value but a pair of reader and writer functions.
sensorRef :: Tuple a => a -> SE (SE a, a -> SE ())
sensorRef a = do
    ref <- newRef (K a)
    return $ (unK <$> readRef ref, writeRef ref . K)
