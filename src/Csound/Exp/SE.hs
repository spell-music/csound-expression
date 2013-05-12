-- | side effects
module Csound.Exp.SE(
    Outs,
    SE(..), LocalHistory(..), 
    se, se_, runSE, execSE, newVar, 
    ifBegin, ifEnd, elseIfBegin, elseBegin,
    writeVar, readVar
) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad(ap)
import Control.Monad.Trans.State.Strict
import Data.Default
import Data.Maybe(fromJust)
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
    , newVarId          :: Int }

instance Default LocalHistory where
    def = LocalHistory def def

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
execSE = fromJust . expDependency . snd . runSE

-- dependency tracking

se :: (Val a) => E -> SE a
se a = SE $ state $ \s -> 
    let x = Fix $ (unFix a) { ratedExpDepends = expDependency s }
    in  (fromE x, s{ expDependency = Just x } )

se_ :: E -> SE ()
se_ = fmap (const ()) . (se :: E -> SE E)

------------------------------------------------------
-- imperative if-then-else

ifBegin :: Val a => a -> SE ()
ifBegin = withCond IfBegin

elseIfBegin :: Val a => a -> SE ()
elseIfBegin = withCond ElseIfBegin

elseBegin :: SE ()
elseBegin = stmtOnly ElseBegin

ifEnd :: SE ()
ifEnd = stmtOnly IfEnd

stmtOnly stmt = se_ $ fromE $ noRate stmt

withCond :: Val a => (E -> MainExp E) -> a -> SE ()
withCond stmt cond = se_ $ fromE $ noRate $ fmap (PrimOr . Right) $ stmt (toE cond)

--------------------------------------------------
-- variables

newVar :: Rate -> SE Var
newVar rate = SE $ state $ \s -> 
    (Var LocalVar rate ("var" ++ show (newVarId s)), s{ newVarId = succ (newVarId s) })

writeVar :: (Val a) => Var -> a -> SE ()
writeVar v x = se_ $ noRate $ WriteVar v $ toPrimOr $ toE x 

readVar :: (Val a) => Var -> a
readVar v = noRate $ ReadVar v

---------------------------------------------------



