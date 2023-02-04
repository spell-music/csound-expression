-- | Mutable references. Which can hold Csound values and
-- allow us to xreate mutable values.
module Csound.Typed.Core.Types.SE.Ref
  ( Ref (..)
  , newRef
  , newCtrlRef
  , readRef
  , writeRef
  , modifyRef
  , sensorRef
  ) where

import Control.Monad
import Control.Monad.Trans.Class (lift)

import Csound.Dynamic (Var, Rate (..))
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.State qualified as State
import Csound.Typed.Core.Types.SE
import Csound.Typed.Core.Types.Tuple

-- | It describes a reference to mutable values.
newtype Ref a = Ref [Var]

newRefBy :: forall a . Tuple a => (Rate -> Rate) -> a -> SE (Ref a)
newRefBy setRate initVals = fmap Ref $ SE $ do
  isGlobal <- lift State.isGlobalInstr
  if isGlobal
    then lift $ zipWithM State.initGlobalVar (fmap setRate $ tupleRates @a) =<< fromTuple initVals
    else Dynamic.newLocalVars (fmap setRate $ tupleRates @a) (fromTuple initVals)

newRef :: forall a . Tuple a => a -> SE (Ref a)
newRef = newRefBy id

-- | Control rate reference.
-- Csound detail: it uses Kr instead of both Ar or Ir
newCtrlRef :: forall a . Tuple a => a -> SE (Ref a)
newCtrlRef = newRefBy toCtrlRate

toCtrlRate :: Rate -> Rate
toCtrlRate = \case
  Ar -> Kr
  Ir -> Kr
  x  -> x

readRef  :: Tuple a => Ref a -> SE a
readRef (Ref vars) = SE $ fmap (toTuple . return) $ mapM Dynamic.readVar vars

writeRef :: Tuple a => Ref a -> a -> SE ()
writeRef (Ref vars) a = SE $ do
  vals <- lift $ fromTuple a
  zipWithM_ Dynamic.writeVar vars vals

-- | Modifies the Ref value with given function.
modifyRef :: Tuple a => Ref a -> (a -> a) -> SE ()
modifyRef ref f = do
    v <- readRef ref
    writeRef ref (f v)

-- | An alias for the function @newRef@. It returns not the reference
-- to mutable value but a pair of reader and writer functions.
sensorRef :: Tuple a => a -> SE (SE a, a -> SE ())
sensorRef a = do
    ref <- newCtrlRef a
    return $ (readRef ref, writeRef ref)
