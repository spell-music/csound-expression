{-# Language CPP #-}
-- | Dependency tracking
module Csound.Dynamic.Types.Dep(
    DepT(..), LocalHistory(..), runDepT, execDepT, evalDepT,
    -- * Dependencies
    depT, depT_, mdepT, stripDepT, stmtOnlyT,

    -- * Variables
    newLocalVar, newLocalVars,
    writeVar, readVar, readOnlyVar, initVar, appendVarBy,

    -- * Arrays
    newLocalArrVar, newTmpArrVar,
    readArr, readOnlyArr, writeArr, writeInitArr, initArr, appendArrBy,

    -- * Read macros
    readMacrosDouble, readMacrosInt, readMacrosString,
    initMacrosDouble, initMacrosString, initMacrosInt
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad(ap, liftM, zipWithM_)
import Data.Default

import Data.Fix(Fix(..))

import Csound.Dynamic.Types.Exp

-- | Csound's synonym for 'IO'-monad. 'Dep' means Side Effect.
-- You will bump into 'Dep' trying to read and write to delay lines,
-- making random signals or trying to save your audio to file.
-- Instrument is expected to return a value of @Dep [Sig]@.
-- So it's okay to do some side effects when playing a note.
newtype DepT m a = DepT { unDepT :: StateT LocalHistory m a }

data LocalHistory = LocalHistory
    { expDependency :: E
    , newLineNum    :: Int
    , newLocalVarId :: Int }

instance Default LocalHistory where
    def = LocalHistory start 0 0

instance Monad m => Functor (DepT m) where
    fmap = liftM

instance Monad m => Applicative (DepT m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (DepT m) where
    return = DepT . return
    ma >>= mf = DepT $ unDepT ma >>= unDepT . mf

instance MonadTrans DepT where
    lift ma = DepT $ lift ma

runDepT :: (Functor m, Monad m) => DepT m a -> m (a, LocalHistory)
runDepT a = runStateT (unDepT $ a) def

evalDepT :: (Functor m, Monad m) => DepT m a -> m a
evalDepT a = evalStateT (unDepT $ a) def

execDepT :: (Functor m, Monad m) => DepT m () -> m E
execDepT a = fmap expDependency $ execStateT (unDepT $ a) def

-- dependency tracking

start :: E
start = noRate Starts

depends :: E -> E -> E
depends a1 a2 = noRate $ Seq (toPrimOr a1) (toPrimOr a2)

depT :: Monad m => E -> DepT m E
depT a = DepT $ do
    s <- get
    let a1 = Fix $ (unFix a) { ratedExpDepends = Just (newLineNum s) }
    put $ s {
        newLineNum = succ $ newLineNum s,
        expDependency = depends (expDependency s) a1 }
    return a1

depT_ :: (Monad m) => E -> DepT m ()
depT_ = fmap (const ()) . depT

mdepT :: (Monad m) => MultiOut [E] -> MultiOut (DepT m [E])
mdepT mas = \n -> mapM depT $ ( $ n) mas

stripDepT :: Monad m => DepT m a -> m a
stripDepT (DepT a) = evalStateT a def

stmtOnlyT :: Monad m => Exp E -> DepT m ()
stmtOnlyT stmt = depT_ $ noRate stmt

-- local variables

newLocalVars :: Monad m => [Rate] -> m [E] -> DepT m [Var]
newLocalVars rs vs = do
    vars <- mapM newVar rs
    zipWithM_ initVar vars =<< lift vs
    return vars

newLocalVar :: Monad m => Rate -> m E -> DepT m Var
newLocalVar rate val = do
    var <- newVar rate
    initVar var =<< lift val
    return var

newVar :: Monad m => Rate -> DepT m Var
newVar rate = DepT $ do
    s <- get
    let v = Var LocalVar rate (show $ newLocalVarId s)
    put $ s { newLocalVarId = succ $ newLocalVarId s }
    return v

--------------------------------------------------
-- variables

-- generic funs

writeVar :: Monad m => Var -> E -> DepT m ()
writeVar v x = depT_ $ noRate $ WriteVar v $ toPrimOr x

readVar :: Monad m => Var -> DepT m E
readVar v = depT $ noRate $ ReadVar v

readOnlyVar :: Var -> E
readOnlyVar v = noRate $ ReadVar v

initVar :: Monad m => Var -> E -> DepT m ()
initVar v x = depT_ $ setRate Ir $ noRate $ InitVar v $ toPrimOr x

appendVarBy :: Monad m => (E -> E -> E) -> Var -> E -> DepT m ()
appendVarBy op v x = writeVar v . op x =<< readVar v

--------------------------------------------------
-- arrays

-- init

newLocalArrVar :: Monad m => Rate -> m [E] -> DepT m Var
newLocalArrVar rate val = do
    var <- newVar rate
    initArr var =<< lift val
    return var

newTmpArrVar :: Monad m => Rate -> DepT m Var
newTmpArrVar rate = newVar rate

-- ops

readArr :: Monad m => Var -> [E] -> DepT m E
readArr v ixs = depT $ noRate $ ReadArr v (fmap toPrimOr ixs)

readOnlyArr :: Var -> [E] -> E
readOnlyArr v ixs = noRate $ ReadArr v (fmap toPrimOr ixs)

writeArr :: Monad m => Var -> [E] -> E -> DepT m ()
writeArr v ixs a = depT_ $ noRate $ WriteArr v (fmap toPrimOr ixs) (toPrimOr a)

writeInitArr :: Monad m => Var -> [E] -> E -> DepT m ()
writeInitArr v ixs a = depT_ $ noRate $ WriteInitArr v (fmap toPrimOr ixs) (toPrimOr a)

initArr :: Monad m => Var -> [E] -> DepT m ()
initArr v xs = depT_ $ noRate $ InitArr v $ fmap toPrimOr xs

appendArrBy :: Monad m => (E -> E -> E) -> Var -> [E] -> E -> DepT m ()
appendArrBy op v ixs x = writeArr v ixs . op x =<< readArr v ixs

--------------------------------------------------
-- read global macros arguments

readMacrosDouble :: String -> E
readMacrosDouble = readMacrosBy ReadMacrosDouble Ir

readMacrosInt :: String -> E
readMacrosInt = readMacrosBy ReadMacrosInt Ir

readMacrosString :: String -> E
readMacrosString = readMacrosBy ReadMacrosString Sr

initMacrosDouble :: Monad m => String -> Double -> DepT m ()
initMacrosDouble = initMacrosBy InitMacrosDouble

initMacrosString :: Monad m => String -> String -> DepT m ()
initMacrosString = initMacrosBy InitMacrosString

initMacrosInt :: Monad m => String -> Int -> DepT m ()
initMacrosInt = initMacrosBy InitMacrosInt

readMacrosBy :: (String -> Exp E) -> Rate -> String -> E
readMacrosBy readMacro rate name = withRate rate $ readMacro name

initMacrosBy :: Monad m => (String -> a -> Exp E) -> String -> a -> DepT m ()
initMacrosBy maker name value = depT_ $ noRate $ maker name value


