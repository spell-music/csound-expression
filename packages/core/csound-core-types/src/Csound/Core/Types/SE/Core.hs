module Csound.Core.Types.SE.Core
  ( SE (..)
  , setTotalDur
  , renderSE
  , global
  , withSetup
  , withOption
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
  , liftOpr1
  , liftOpr1k
  , liftOpr1kDep
  , readOnlyVar
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Kind
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Default
import Control.Monad.Trans.Class (lift)

import Csound.Dynamic (IfRate (..), Rate (..), E, Name, Spec1)
import Csound.Dynamic qualified as Dynamic
import Csound.Core.State (Dep)
import Csound.Core.Render.Options (Options (..), addUdo, UdoDef (..))
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

renderSE :: Options -> SE () -> IO Text
renderSE config (SE act) = do
  result <- fmap (Dynamic.renderCsd def) $ State.exec config $ do
    mainInstr <- Dynamic.execDepT act
    instrId <- State.insertInstr mainInstr
    State.insertNote instrId (0, -1, [])
  saveCsd result
  pure result
  where
    saveCsd :: Text -> IO ()
    saveCsd result =
      mapM_ (\file -> Text.writeFile file result) config.csdWriteFile

-- | It is executed inside intr 0 (setup instrument) so the If-rate is always Ir.
global :: forall a . Tuple a => SE a -> SE a
global (SE expr) = SE $ do
  vars <- lift $ do
    initVals <- fromTuple (defTuple @a)
    vars <- zipWithM State.initGlobalVar (tupleRates @a) initVals
    ge <- Dynamic.execDepT $ do
      exprs <- lift . fromTuple =<< expr
      zipWithM_ (Dynamic.writeVar IfIr) vars exprs
    State.insertGlobalExpr ge
    pure vars
  pure $ toTuple $ pure $ fmap (Dynamic.readOnlyVar IfIr) vars

withSetup :: forall a b . (Tuple a) => SE a -> (a -> b) -> b
withSetup (SE setupExpr) cont = cont $ toTuple $ do
  initVals <- fromTuple (defTuple @a)
  State.getReadOnlyVars (tupleRates @a) initVals (lift . fromTuple =<< setupExpr)

withOption :: forall a . Tuple a => Options -> a -> a
withOption opt a = toTuple $ do
  exprs <- fromTuple a
  State.setOption opt
  pure exprs

-- | forces set of the option to new value
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
  State.getReadOnlyVar (Dynamic.toInitRate $ valRate @a) e

------------------------------------------------------------------------------------
-- use csound opcodes

liftOpc :: (FromTuple a, Val b) => Name -> Spec1 -> a -> b
liftOpc name rates a = fromE $ Dynamic.opcs name rates <$> fromTuple a

liftMulti :: forall a b . (FromTuple a, Tuple b) => Name -> ([Rate], [Rate]) -> a -> b
liftMulti name rates a = pureTuple $ Dynamic.mopcs name rates <$> fromTuple a
  where
    pureTuple outs = toTuple $ fmap ($ tupleArity @b) outs

liftOpcDep :: (FromTuple a, Val b) => Name -> Spec1 -> a -> SE b
liftOpcDep name rates a = SE $ fmap (fromE . pure) $ Dynamic.opcsDep name rates =<< lift (fromTuple a)

liftOpcDep_ :: (FromTuple a) => Name -> Spec1 -> a -> SE ()
liftOpcDep_ name rates a = SE $ Dynamic.opcsDep_ name rates =<< lift (fromTuple a)

-- | TODO: Multi+dep produces bug
liftMultiDep :: forall a b . (FromTuple a, Tuple b) => Name -> ([Rate], [Rate]) -> a -> SE b
liftMultiDep name rates a =
  SE $ fmap (toTuple . pure) $ Dynamic.mopcsDep (tupleArity @b) name rates =<< lift (fromTuple a)

liftOpr1 :: (Val a, Val b) => Name -> a -> b
liftOpr1 name a = fromE $ Dynamic.opr1 name <$> toE a

liftOpr1k :: (Val a, Val b) => Name -> a -> b
liftOpr1k name a = fromE $ Dynamic.opr1k name <$> toE a

liftOpr1kDep :: (Val a, Val b) => Name -> a -> SE b
liftOpr1kDep name b1 = SE $ fmap (fromE . pure) $ Dynamic.opr1kDep name =<< lift (toE b1)
