-- | side effects
module Csound.Exp.SE(
    Outs,
    SE(..), LocalHistory(..), 
    se, se_, stmtOnly, runSE, execSE, 
    writeVar, readVar, readOnlyVar, initVar, appendVar, appendVarBy, 
    newLocalVar
) where

import Control.Applicative
import Control.Monad(ap)
import Control.Monad.Trans.State.Strict
import Data.Default
import Data.Maybe(fromJust)
import Data.Monoid
import Data.Fix(Fix(..))

import Csound.Exp
import Csound.Exp.Wrapper

type Outs = SE [Sig]

-- | Csound's synonym for 'IO'-monad. 'SE' means Side Effect. 
-- You will bump into 'SE' trying to read and write to delay lines,
-- making random signals or trying to save your audio to file. 
-- Instrument is expected to return a value of @SE [Sig]@. 
-- So it's okay to do some side effects when playing a note.
newtype SE a = SE { unSE :: State LocalHistory a }

data LocalHistory = LocalHistory
    { expDependency     :: Maybe E
    , newVarId          :: InitVarId }


instance Default LocalHistory where
    def = LocalHistory def def

type InitVarId = Int

instance Functor SE where
    fmap f = SE . fmap f . unSE

instance Applicative SE where
    pure = return
    (<*>) = ap

instance Monad SE where
    return = SE . return
    ma >>= mf = SE $ unSE ma >>= unSE . mf

runSE :: SE a -> (a, LocalHistory)
runSE a = runState (unSE a) def

execSE :: SE a -> E
execSE a = expr
    where st = snd $ runSE a
          expr  = fromJust $ expDependency st

-- dependency tracking

se :: (Val a) => E -> SE a
se a = SE $ state $ \s -> 
    let x = Fix $ (unFix a) { ratedExpDepends = expDependency s }
    in  (fromE x, s{ expDependency = Just x } )

se_ :: E -> SE ()
se_ = fmap (const ()) . (se :: E -> SE E)

stmtOnly :: Exp E -> SE ()
stmtOnly stmt = se_ $ fromE $ noRate stmt

dependsOn :: E -> E -> E
dependsOn expr dep = case ratedExpDepends (unFix expr) of
    Nothing -> insertDep dep
    Just x  -> insertDep (dependsOn x dep) 
    where insertDep a = Fix $ (unFix expr) { ratedExpDepends = Just a }

--------------------------------------------------
-- variables

-- generic funs

writeVar :: (Val a) => Var -> a -> SE ()
writeVar v x = se_ $ noRate $ WriteVar v $ toPrimOr $ toE x 

readVar :: (Val a) => Var -> SE a
readVar v = se $ noRate $ ReadVar v

readOnlyVar :: (Val a) => Var -> a
readOnlyVar v = noRate $ ReadVar v

initVar :: (Val a) => Var -> a -> SE ()
initVar v x = se_ $ noRate $ InitVar v $ toPrimOr $ toE x

appendVar :: (Monoid a, Val a) => Var -> a -> SE ()
appendVar = appendVarBy mappend

appendVarBy :: (Val a) => (a -> a -> a) -> Var -> a -> SE ()
appendVarBy op v x = writeVar v . op x =<< readVar v

-- new local variables

newLocalVar :: Val a => Rate -> a -> SE Var
newLocalVar rate val = SE $ do
    s <- get     
    let v = Var LocalVar rate (show $ newVarId s)    
        initStmt = noRate $ InitVar v $ toPrimOr $ toE val
        dep = case expDependency s of
            Nothing -> Just initStmt 
            Just x  -> Just $ x `dependsOn` initStmt
    put $ s { expDependency = dep, newVarId = succ $ newVarId s }
    return v

