-- | Mutable references. Which can hold Csound values and
-- allow us to xreate mutable values.
module Csound.Typed.Core.Types.SE.Ref
  ( Ref (..)
  , newRef
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

newRef :: Tuple a => a -> SE (Ref a)
newRef = newRefBy id

instance IsRef Ref where

  readRef (Ref vars) = SE $ fmap (toTuple . return) $ mapM Dynamic.readVar vars

  writeRef (Ref vars) a = SE $ do
    vals <- lift $ fromTuple a
    zipWithM_ Dynamic.writeVar vars vals
