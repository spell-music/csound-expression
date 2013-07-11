-- | side effects
module Csound.Exp.SE(
    Outs,
    SE(..), LocalHistory(..), 
    se, se_, stmtOnly, runSE, execSE, 
    writeVar, readVar, initVar, appendVar, appendVarBy, 
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
    , locals            :: Locals }


instance Default LocalHistory where
    def = LocalHistory def def

data Locals = Locals 
    { newVarId          :: Int
    , localInits        :: [SE ()] }

instance Default Locals where
    def = Locals def def

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
execSE a 
    | null initList = expr
    | otherwise     = execSE $ applyInitList initList expr >> clearInitList
    where st = snd $ runSE a
          expr = fromJust $ expDependency st
          initList = localInits $ locals st
          clearInitList = SE $ modify $ \s -> s{ locals = def }
          applyInitList inits xs = sequence_ inits >> se_ xs

-- dependency tracking

se :: (Val a) => E -> SE a
se a = SE $ state $ \s -> 
    let x = Fix $ (unFix a) { ratedExpDepends = expDependency s }
    in  (fromE x, s{ expDependency = Just x } )

se_ :: E -> SE ()
se_ = fmap (const ()) . (se :: E -> SE E)

stmtOnly :: Exp E -> SE ()
stmtOnly stmt = se_ $ fromE $ noRate stmt


--------------------------------------------------
-- variables

-- generic funs

writeVar :: (Val a) => Var -> a -> SE ()
writeVar v x = se_ $ noRate $ WriteVar v $ toPrimOr $ toE x 

readVar :: (Val a) => Var -> a
readVar v = noRate $ ReadVar v

initVar :: (Val a) => Var -> a -> SE ()
initVar v x = se_ $ noRate $ InitVar v $ toPrimOr $ toE x

appendVar :: (Monoid a, Val a) => Var -> a -> SE ()
appendVar = appendVarBy mappend

appendVarBy :: (Val a) => (a -> a -> a) -> Var -> a -> SE ()
appendVarBy op v x = writeVar v $ readVar v `op` x

-- new local variables

newLocalVar :: Val a => Rate -> a -> SE Var
newLocalVar rate initVal = SE $ do
    s <- get
    let (var, locals') = newVarOnLocals rate initVal (locals s)
    put $ s { locals = locals' }
    return var

newVarOnLocals :: Val a => Rate -> a -> Locals -> (Var, Locals)
newVarOnLocals rate initVal st = 
    (var, st { newVarId = succ n, localInits = initStmt : localInits st })
    where var = Var LocalVar rate (show n)
          n = newVarId st  
          initStmt = initVar var initVal

