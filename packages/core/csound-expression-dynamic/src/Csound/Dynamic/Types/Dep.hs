{-# LANGUAGE CPP #-}

-- | Dependency tracking
module Csound.Dynamic.Types.Dep (
  DepT (..),
  LocalHistory (..),
  runDepT,
  execDepT,
  evalDepT,

  -- * Dependencies
  depT_,
  stripDepT,
  stmtOnlyT,
  depends,
  tfmDep,
  tfmDepVar,

  -- * Variables
  newLocalVar,
  newLocalVars,
  writeVar,
  readVar,
  readOnlyVar,
  initVar,
  appendVarBy,

  -- * Arrays
  newLocalArrVar,
  newTmpArrVar,
  readArr,
  readOnlyArr,
  writeArr,
  writeInitArr,
  initArr,
  appendArrBy,

  -- * Read macros
  readMacrosDouble,
  readMacrosInt,
  readMacrosString,
  initMacrosDouble,
  initMacrosString,
  initMacrosInt,
) where

import Control.Monad (ap, liftM, zipWithM_)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Default

import Data.Fix (Fix (..))
import Data.Text (Text)
import Data.Text qualified as Text

import Csound.Dynamic.Types.Exp

{-| Csound's synonym for 'IO'-monad. 'Dep' means Side Effect.
You will bump into 'Dep' trying to read and write to delay lines,
making random signals or trying to save your audio to file.
Instrument is expected to return a value of @Dep [Sig]@.
So it's okay to do some side effects when playing a note.
-}
newtype DepT m a = DepT {unDepT :: StateT LocalHistory m a}

data LocalHistory = LocalHistory
  { expDependency :: E
  , newLineNum :: Int
  , newLocalVarId :: Int
  , newTmpVarNum :: Int
  }

instance Default LocalHistory where
  def = LocalHistory (noRate Starts) 0 0 0

instance (Monad m) => Functor (DepT m) where
  fmap = liftM

instance (Monad m) => Applicative (DepT m) where
  pure = DepT . return
  (<*>) = ap

instance (Monad m) => Monad (DepT m) where
  ma >>= mf = DepT $ unDepT ma >>= unDepT . mf

instance MonadTrans DepT where
  lift ma = DepT $ lift ma

runDepT :: DepT m a -> m (a, LocalHistory)
runDepT a = runStateT (unDepT $ a) def

evalDepT :: (Monad m) => DepT m a -> m a
evalDepT a = evalStateT (unDepT $ a) def

execDepT :: (Monad m) => DepT m a -> m E
execDepT a = fmap expDependency $ execStateT (unDepT $ a) def

-- dependency tracking

depends :: E -> E -> E
depends a1 a2 =
  case ratedExpExp (unFix a2) of
    Starts -> a1
    _ ->
      case ratedExpExp (unFix a1) of
        Starts -> a2
        _ -> noRate $ Seq (toPrimOr a1) (toPrimOr a2)

tfmDep :: (Monad m) => Info -> [E] -> DepT m E
tfmDep info args = fmap fromTmpVar $ tfmDepVar info args

tfmDepVar :: (Monad m) => Info -> [E] -> DepT m TmpVar
tfmDepVar info args = do
  v <- getNewTmpVar (Just info)
  depT_ $ tfmInit v info args
  pure v

tfmInit :: TmpVar -> Info -> [E] -> E
tfmInit v info args = noRate $ TfmInit v info $ toArgs (getInfoRates info) args

toArgs :: [Rate] -> [E] -> [PrimOr E]
toArgs = zipWith toPrimOrTfm

getNewTmpVar :: (Monad m) => Maybe Info -> DepT m TmpVar
getNewTmpVar mInfo = DepT $ do
  n <- gets newTmpVarNum
  modify' $ \s -> s{newTmpVarNum = n + 1}
  pure (TmpVar Nothing mInfo n)

depT_ :: (Monad m) => E -> DepT m ()
depT_ a =
  -- fmap (const ()) . depT
  DepT $ do
    s <- get
    let
      a1 = rehashE $ Fix $ (unFix a){ratedExpDepends = Just (newLineNum s)}
    put $
      s
        { newLineNum = succ $ newLineNum s
        , expDependency = depends (expDependency s) a1
        }

stripDepT :: (Monad m) => DepT m a -> m a
stripDepT (DepT a) = evalStateT a def

stmtOnlyT :: (Monad m) => Exp E -> DepT m ()
stmtOnlyT stmt = depT_ $ noRate stmt

-- local variables

newLocalVars :: (Monad m) => [Rate] -> m [E] -> DepT m [Var]
newLocalVars rs vs = do
  vars <- mapM newVar rs
  zipWithM_ initVar vars =<< lift vs
  return vars

newLocalVar :: (Monad m) => Rate -> m E -> DepT m Var
newLocalVar rate val = do
  var <- newVar rate
  initVar var =<< lift val
  return var

newVar :: (Monad m) => Rate -> DepT m Var
newVar rate = DepT $ do
  s <- get
  let
    v = Var LocalVar rate (Text.pack $ show $ newLocalVarId s)
  put $ s{newLocalVarId = succ $ newLocalVarId s}
  return v

--------------------------------------------------
-- variables

-- generic funs

writeVar :: (Monad m) => IfRate -> Var -> E -> DepT m ()
writeVar ifRate v x = depT_ $ noRate $ WriteVar ifRate v $ toPrimOr x

readVar :: (Monad m) => IfRate -> Var -> DepT m E
readVar ifRate v = do
  tmp <- getNewTmpVar Nothing
  depT_ $ noRate $ ReadVarTmp ifRate tmp v
  pure $ fromTmpVar tmp

fromTmpVar :: TmpVar -> E
fromTmpVar v = noRate $ ExpPrim $ PrimTmpVar v

readOnlyVar :: IfRate -> Var -> E
readOnlyVar ifRate v = noRate $ ReadVar ifRate v

initVar :: (Monad m) => Var -> E -> DepT m ()
initVar v x = depT_ $ noRate $ InitVar v $ toPrimOr $ setRate Ir x

appendVarBy :: (Monad m) => (E -> E -> E) -> IfRate -> Var -> E -> DepT m ()
appendVarBy op ifRate v x = writeVar ifRate v . op x =<< readVar ifRate v

--------------------------------------------------
-- arrays

-- init

newLocalArrVar :: (Monad m) => Rate -> m [E] -> DepT m Var
newLocalArrVar rate val = do
  var <- newVar rate
  initArr var =<< lift val
  return var

newTmpArrVar :: (Monad m) => Rate -> DepT m Var
newTmpArrVar rate = newVar rate

-- ops

readArr :: (Monad m) => IfRate -> Var -> [E] -> DepT m E
readArr ifRate v ixs = do
  tmp <- getNewTmpVar Nothing
  depT_ $ noRate $ ReadArrTmp ifRate tmp v (fmap toPrimOr ixs)
  pure $ fromTmpVar tmp

readOnlyArr :: IfRate -> Var -> [E] -> E
readOnlyArr ifRate v ixs = noRate $ ReadArr ifRate v (fmap toPrimOr ixs)

writeArr :: (Monad m) => IfRate -> Var -> [E] -> E -> DepT m ()
writeArr ifRate v ixs a = depT_ $ noRate $ WriteArr ifRate v (fmap toPrimOr ixs) (toPrimOr a)

writeInitArr :: (Monad m) => IfRate -> Var -> [E] -> E -> DepT m ()
writeInitArr ifRate v ixs a = depT_ $ noRate $ WriteInitArr ifRate v (fmap toPrimOr ixs) (toPrimOr a)

initArr :: (Monad m) => Var -> [E] -> DepT m ()
initArr v xs = depT_ $ noRate $ InitArr v $ fmap toPrimOr xs

appendArrBy :: (Monad m) => (E -> E -> E) -> IfRate -> Var -> [E] -> E -> DepT m ()
appendArrBy op ifRate v ixs x = writeArr ifRate v ixs . op x =<< readArr ifRate v ixs

--------------------------------------------------
-- read global macros arguments

readMacrosDouble :: Text -> E
readMacrosDouble = readMacrosBy ReadMacrosDouble Ir

readMacrosInt :: Text -> E
readMacrosInt = readMacrosBy ReadMacrosInt Ir

readMacrosString :: Text -> E
readMacrosString = readMacrosBy ReadMacrosString Sr

initMacrosDouble :: (Monad m) => Text -> Double -> DepT m ()
initMacrosDouble = initMacrosBy InitMacrosDouble

initMacrosString :: (Monad m) => Text -> Text -> DepT m ()
initMacrosString = initMacrosBy InitMacrosString

initMacrosInt :: (Monad m) => Text -> Int -> DepT m ()
initMacrosInt = initMacrosBy InitMacrosInt

readMacrosBy :: (Text -> Exp E) -> Rate -> Text -> E
readMacrosBy readMacro rate name = withRate rate $ readMacro name

initMacrosBy :: (Monad m) => (Text -> a -> Exp E) -> Text -> a -> DepT m ()
initMacrosBy maker name value = depT_ $ noRate $ maker name value
