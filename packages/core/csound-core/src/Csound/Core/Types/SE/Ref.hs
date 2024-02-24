-- | Mutable references. Which can hold Csound values and
-- allow us to xreate mutable values.
module Csound.Core.Types.SE.Ref
  ( Ref (..)
  , newRef
  , newCtrlRef
  , newInitRef
  , newLocalRef
  , newLocalCtrlRef
  , newLocalInitRef
  ) where

import Control.Monad
import Control.Monad.Trans.Class (lift)

import Csound.Dynamic (Var, E, Rate)
import Csound.Dynamic qualified as Dynamic
import Csound.Core.State (Dep, Run)
import Csound.Core.State qualified as State
import Csound.Core.Types.SE.Core
import Csound.Core.Types.Tuple

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
newRef = newRefBy id initGlobalVars

-- | Creates global mutable variable (reference). It can be shared between different
-- instances ofthe instruments. If type is signal it creates K-rate signals
newCtrlRef :: forall a . Tuple a => a -> SE (Ref a)
newCtrlRef = newRefBy toCtrlRate initGlobalVars

-- | Creates global mutable variable (reference). It can be shared between different
-- instances ofthe instruments. If type is signal or constant it creates I-rate variable
newInitRef :: forall a . Tuple a => a -> SE (Ref a)
newInitRef = newRefBy toInitRate initGlobalVars

initGlobalVars :: [Rate] -> Run [E] -> Dep [Var]
initGlobalVars rates vals = lift $ zipWithM State.initGlobalVar rates =<< vals

-- | Creates local mutable variable (reference). It can not be shared between different local instruments
newLocalRef :: forall a . Tuple a => a -> SE (Ref a)
newLocalRef = newRefBy id Dynamic.newLocalVars

-- | Creates local mutable variable (reference). It can not be shared between different local instruments
newLocalCtrlRef :: forall a . Tuple a => a -> SE (Ref a)
newLocalCtrlRef = newRefBy toCtrlRate Dynamic.newLocalVars

-- | Creates local mutable variable (reference). It can not be shared between different local instruments
newLocalInitRef :: forall a . Tuple a => a -> SE (Ref a)
newLocalInitRef = newRefBy toInitRate Dynamic.newLocalVars

-- | Creates local mutable variable (reference). It can not be shared between different local instruments
newRefBy :: forall a . Tuple a => (Rate -> Rate) -> ([Rate] -> Run [E] -> Dep [Var]) -> a -> SE (Ref a)
newRefBy rateFun initVars initVals =
  fmap Ref $ SE $ initVars (fmap rateFun $ tupleRates @a) (fromTuple initVals)

instance IsRef Ref where
  readRef (Ref vars) = SE $ fmap (toTuple . return) $ mapM Dynamic.readVar vars

  writeRef (Ref vars) a = SE $ do
    vals <- lift $ fromTuple a
    zipWithM_ Dynamic.writeVar vars vals

toCtrlRate :: Dynamic.Rate -> Dynamic.Rate
toCtrlRate x = case x of
    Dynamic.Ar -> Dynamic.Kr
    _  -> x

toInitRate :: Dynamic.Rate -> Dynamic.Rate
toInitRate x = case x of
    Dynamic.Ar -> Dynamic.Ir
    Dynamic.Kr -> Dynamic.Ir
    _  -> x
