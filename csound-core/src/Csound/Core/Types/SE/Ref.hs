-- | Mutable references. Which can hold Csound values and
-- allow us to xreate mutable values.
module Csound.Typed.Core.Types.SE.Ref
  ( Ref (..)
  , newRef
  , newLocalRef
  ) where

import Control.Monad
import Control.Monad.Trans.Class (lift)

import Csound.Dynamic (Var)
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.State qualified as State
import Csound.Typed.Core.Types.SE
import Csound.Typed.Core.Types.Tuple

-- | It describes a reference to mutable values.
newtype Ref a = Ref [Var]

{-
-- | It creates global variables if it is used from top level instrument
-- or global variable if it's used inside local instrument
--
-- Be careful that local variables can not be shared (via closures)
-- between different instances of the instruments. It will compile and create broken Csound code.
-- Only globals can be shared.
newRef :: forall a . Tuple a => a -> SE (Ref a)
newRef initVals = do
  isGlobal <- SE (lift State.isGlobalInstr)
  if isGlobal
    then newGlobalRef initVals
    else newLocalRef initVals
-}

-- | Creates global mutable variable (reference). It can be shared between different
-- instances ofthe instruments.
newRef :: forall a . Tuple a => a -> SE (Ref a)
newRef initVals =
  fmap Ref $ SE $ lift $ zipWithM State.initGlobalVar (tupleRates @a) =<< fromTuple initVals

-- | Creates local mutable variable (reference). It can not be shared between different local instruments
newLocalRef :: forall a . Tuple a => a -> SE (Ref a)
newLocalRef initVals =
  fmap Ref $ SE $ Dynamic.newLocalVars (tupleRates @a) (fromTuple initVals)

instance IsRef Ref where
  readRef (Ref vars) = SE $ fmap (toTuple . return) $ mapM Dynamic.readVar vars

  writeRef (Ref vars) a = SE $ do
    vals <- lift $ fromTuple a
    zipWithM_ Dynamic.writeVar vars vals
