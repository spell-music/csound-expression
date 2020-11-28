module Csound.Typed.GlobalState.SE(
    SE(..), LocalHistory(..), 
    runSE, execSE, evalSE, execGEinSE, hideGEinDep, 
    fromDep, fromDep_, geToSe,
    newLocalVar, newLocalVars, newGlobalVars, newClearableGlobalVars,
    -- array variables
    newLocalArrVar, newGlobalArrVar, newTmpArrVar
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

import Csound.Dynamic hiding (newLocalVar, newLocalVars, newLocalArrVar, newTmpArrVar)
import qualified Csound.Dynamic as D(newLocalVar, newLocalVars, newLocalArrVar, newTmpArrVar)
import Csound.Typed.GlobalState.GE
import Csound.Typed.GlobalState.Elements(newPersistentGlobalVar, newClearableGlobalVar, newPersistentGloabalArrVar)

-- | The Csound's @IO@-monad. All values that produce side effects are wrapped
-- in the @SE@-monad.
newtype SE a = SE { unSE :: Dep a }

instance Functor SE where
    fmap f = SE . fmap f . unSE

instance Applicative SE where
    pure = return
    (<*>) = ap

instance Monad SE where
    return = SE . return
    ma >>= mf = SE $ unSE ma >>= unSE . mf

runSE :: SE a -> GE a
runSE = fmap fst . runDepT . unSE

execSE :: SE () -> GE InstrBody
execSE a = execDepT $ unSE a

execGEinSE :: SE (GE a) -> SE a
execGEinSE a = geToSe =<< a
{-
(SE sa) = SE $ do
    ga <- sa
    a  <- lift ga
    return a
-}

hideGEinDep :: GE (Dep a) -> Dep a
hideGEinDep = join . lift

fromDep :: Dep a -> SE (GE a)
fromDep = fmap return . SE 

fromDep_ :: Dep () -> SE ()
fromDep_ = SE
            
evalSE :: SE a -> GE a
evalSE = evalDepT . unSE

geToSe :: GE a -> SE a
geToSe = SE . lift

----------------------------------------------------------------------
-- allocation of the local vars

newLocalVars :: [Rate] -> GE [E] -> SE [Var]
newLocalVars rs vs = SE $ D.newLocalVars rs vs

newLocalVar :: Rate -> GE E -> SE Var
newLocalVar rate val = SE $ D.newLocalVar rate val

----------------------------------------------------------------------
-- allocation of the global vars

newGlobalVars :: [Rate] -> GE [E] -> SE [Var]
newGlobalVars rs vs = geToSe $ zipWithM f rs =<< vs
    where f r v = onGlobals $ newPersistentGlobalVar r v

newClearableGlobalVars :: [Rate] -> GE [E] -> SE [Var]
newClearableGlobalVars rs vs = geToSe $ zipWithM f rs =<< vs
    where f r v = onGlobals $ newClearableGlobalVar r v

------------------------------------------------------------------
-- allocation of array vars

newLocalArrVar :: Rate -> GE [E] -> SE Var
newLocalArrVar rate val = SE $ D.newLocalArrVar rate val

newTmpArrVar :: Rate -> SE Var
newTmpArrVar rate = SE $ D.newTmpArrVar rate

newGlobalArrVar :: Rate -> GE [E] -> SE Var
newGlobalArrVar r v = geToSe $ onGlobals . newPersistentGloabalArrVar r =<< v
