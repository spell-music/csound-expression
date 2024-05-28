module Csound.Core.Types.SE.Core
  ( SE (..)
  , setTotalDur
  , renderSE
  , global
  , setOption
  , setDefaultOption
  , IsRef (..)
  , InitType
  , modifyRef
  , modifyInitRef
  , getCurrentRate
  , writeOuts
  , readIns
  -- * Opcodes and operators
  , liftOpc
  , liftMulti
  , liftMultiDep
  , liftOpcDep
  , liftOpcDep_
  , liftOpr1kDep
  -- * UDOs
  , liftUdo
  , liftMultiUdo
  , liftUdoDep
  , liftUdoDep_
  , liftMultiUdoDep
  -- * Internal UDOs (defined within the package)
  , liftInternalUdo
  , liftInternalMultiUdo
  , readOnlyVar
  ) where

import Control.Monad.IO.Class
import Data.Kind
import Data.Text (Text)
import Data.Default
import Control.Monad.Trans.Class (lift)

import Csound.Dynamic (IfRate (..), Rate (..), E, Name, Spec1)
import Csound.Dynamic qualified as Dynamic
import Csound.Core.State (Dep)
import Csound.Core.Render.Options (Options)
import Csound.Core.State qualified as State
import Csound.Core.Types.Tuple
import Csound.Core.Types.Prim.Val

newtype SE a = SE { unSE :: Dep a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadFail SE where
  fail = error "no implementation for MonadFail"

instance MonadIO SE where
  liftIO = SE . lift . liftIO

setTotalDur :: Double -> SE a -> SE a
setTotalDur duration (SE act) = SE $ do
  lift $ State.setTotalDur duration
  act

renderSE :: Options -> SE () -> IO String
renderSE config (SE act) = fmap (Dynamic.renderCsd def) $ State.exec config $ do
  mainInstr <- Dynamic.execDepT act
  instrId <- State.insertInstr mainInstr
  State.insertNote instrId (0, -1, [])

-- | Adds expression to the global scope.
-- It is instrument 0 in csound terms.
global :: SE () -> SE ()
global (SE expr) = SE $ lift $ do
  ge <- Dynamic.execDepT expr
  State.insertGlobalExpr ge

-- | forces set of the option option to new value
setOption :: Options -> SE ()
setOption opt = SE $ lift $ State.setOption opt

-- | Sets option if it's not already set
setDefaultOption :: Options -> SE ()
setDefaultOption opt = SE $ lift $ State.setDefaultOption opt

type family InitType a :: Type

class IsRef ref where
  readRef  :: Tuple a => ref a -> SE a
  writeRef :: Tuple a => ref a -> a -> SE ()

  readInitRef  :: (Tuple a, Tuple (InitType a)) => ref a -> SE (InitType a)
  writeInitRef :: (Tuple a, Tuple (InitType a)) => ref a -> (InitType a) -> SE ()

  mixRef   :: (Num a, Tuple a) => ref a -> a -> SE ()
  clearRef :: (Num a, Tuple a) => ref a -> SE ()

  mixRef ref a = modifyRef ref (a +)
  clearRef ref = writeRef ref 0

modifyRef :: (Tuple a, IsRef ref) => ref a -> (a -> a) -> SE ()
modifyRef ref f = writeRef ref . f =<< readRef ref

modifyInitRef :: (Tuple a, Tuple (InitType a), IsRef ref) => ref a -> (InitType a -> InitType a) -> SE ()
modifyInitRef ref f = writeInitRef ref . f =<< readInitRef ref

getCurrentRate :: SE (Maybe IfRate)
getCurrentRate = SE $ lift State.getCurrentRate

-----------------------------------------------------------------------
-- writing and reading signals from audio card

writeOuts :: forall a . Sigs a => a -> SE ()
writeOuts outs = SE $ (Dynamic.depT_ =<<) $ lift $ f <$> fromTuple outs
  where f as = Dynamic.opcs "out" [(Xr, replicate (tupleArity @a) Ar)] as

readIns :: forall a . Sigs a => SE a
readIns = SE $ toTuple . pure <$> getIn (tupleArity @a)
  where
    getIn :: Int -> Dep [E]
    getIn arity
        | arity == 0    = pure []
        | otherwise     = Dynamic.mopcsDep arity "inch" (replicate arity Ar, replicate arity Kr) (fmap Dynamic.int [1 .. arity])

------------------------------------------------------------------------------------

-- | Expression is written in global instrument and only once
readOnlyVar :: forall a . Val a => a -> a
readOnlyVar expr = fromE $ do
  e <- toE expr
  State.getReadOnlyVar IfIr (Dynamic.toInitRate $ valRate @a) e

------------------------------------------------------------------------------------
-- use csound opcodes

liftOpc :: (Tuple a, Val b) => Name -> Spec1 -> a -> b
liftOpc name rates a = fromE $ Dynamic.opcs name rates <$> fromTuple a

liftMulti :: forall a b . (Tuple a, Tuple b) => Name -> ([Rate], [Rate]) -> a -> b
liftMulti name rates a = pureTuple $ Dynamic.mopcs name rates <$> fromTuple a
  where
    pureTuple outs = toTuple $ fmap ($ tupleArity @b) outs

liftOpcDep :: (Tuple a, Val b) => Name -> Spec1 -> a -> SE b
liftOpcDep name rates a = SE $ fmap (fromE . pure) $ Dynamic.opcsDep name rates =<< lift (fromTuple a)

liftOpcDep_ :: (Tuple a) => Name -> Spec1 -> a -> SE ()
liftOpcDep_ name rates a = SE $ Dynamic.opcsDep_ name rates =<< lift (fromTuple a)

-- | TODO: Multi+dep produces bug
liftMultiDep :: forall a b . (Tuple a, Tuple b) => Name -> ([Rate], [Rate]) -> a -> SE b
liftMultiDep name rates a = SE $ fmap (toTuple . pure) $ Dynamic.mopcsDep (tupleArity @b) name rates =<< lift (fromTuple a)

liftOpr1kDep :: (Val a, Val b) => Name -> a -> SE b
liftOpr1kDep name b1 = SE $ fmap (fromE . pure) $ Dynamic.opr1kDep name =<< lift (toE b1)

-- * UDOs

liftUdo :: (Tuple a, Val b) => FilePath -> Name -> Spec1 -> a -> b
liftUdo file name rates a = fromE $ do
  State.includeUdoFile name file
  Dynamic.opcs name rates <$> fromTuple a

liftMultiUdo :: forall a b . (Tuple a, Tuple b) => FilePath -> Name -> ([Rate], [Rate]) -> a -> b
liftMultiUdo file name rates a = pureTuple $ do
  State.includeUdoFile name file
  Dynamic.mopcs name rates <$> fromTuple a
  where
    pureTuple outs = toTuple $ fmap ($ tupleArity @b) outs

liftUdoDep :: (Tuple a, Val b) => FilePath -> Name -> Spec1 -> a -> SE b
liftUdoDep file name rates a = SE $ fmap (fromE . pure) $ do
  lift (State.includeUdoFile name file)
  Dynamic.opcsDep name rates =<< lift (fromTuple a)

liftUdoDep_ :: (Tuple a) => FilePath -> Name -> Spec1 -> a -> SE ()
liftUdoDep_ file name rates a = SE $ do
  lift (State.includeUdoFile name file)
  Dynamic.opcsDep_ name rates =<< lift (fromTuple a)

liftMultiUdoDep :: forall a b . (Tuple a, Tuple b) => FilePath -> Name -> ([Rate], [Rate]) -> a -> SE b
liftMultiUdoDep file name rates a = SE $ do
  lift (State.includeUdoFile name file)
  fmap (toTuple . pure) $ Dynamic.mopcsDep (tupleArity @b) name rates =<< lift (fromTuple a)

-- * Internal UDOs

liftInternalUdo :: (Tuple a, Val b) => Text -> Name -> Spec1 -> a -> b
liftInternalUdo udoContent name rates a = fromE $ do
  State.includeUdo name udoContent
  Dynamic.opcs name rates <$> fromTuple a

liftInternalMultiUdo :: forall a b . (Tuple a, Tuple b) => Text -> Name -> ([Rate], [Rate]) -> a -> b
liftInternalMultiUdo udoContent name rates a = pureTuple $ do
  State.includeUdo name udoContent
  Dynamic.mopcs name rates <$> fromTuple a
  where
    pureTuple outs = toTuple $ fmap ($ tupleArity @b) outs
