module Csound.Typed.Core.Types.SE
  ( SE (..)
  , setTotalDur
  , renderSE
  , global
  , IsRef (..)
  , modifyRef
  , getCurrentRate
  , writeOuts
  , readIns
  , liftOpc
  , liftMulti
  , liftMultiDep
  , liftOpcDep
  , liftOpcDep_
  , liftOpr1kDep
  , readOnlyVar
  ) where

import Control.Monad.IO.Class

import Csound.Dynamic (IfRate (..), Rate (..), E, Name, Spec1)
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.State (Dep)
import Csound.Typed.Core.State.Options (Options)
import Csound.Typed.Core.State qualified as State
import Csound.Typed.Core.Types.Tuple
import Csound.Typed.Core.Types.Prim.Val
import Control.Monad.Trans.Class (lift)
import Data.Default

newtype SE a = SE { unSE :: Dep a }
  deriving newtype (Functor, Applicative, Monad)

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

class IsRef ref where
  readRef  :: Tuple a => ref a -> SE a
  writeRef :: Tuple a => ref a -> a -> SE ()

  mixRef   :: (Num a, Tuple a) => ref a -> a -> SE ()
  clearRef :: (Num a, Tuple a) => ref a -> SE ()

  mixRef ref a = modifyRef ref (a +)
  clearRef ref = writeRef ref 0

modifyRef :: (Tuple a, IsRef ref) => ref a -> (a -> a) -> SE ()
modifyRef ref f = writeRef ref . f =<< readRef ref

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

liftMultiDep :: forall a b . (Tuple a, Tuple b) => Name -> ([Rate], [Rate]) -> a -> SE b
liftMultiDep name rates a = SE $ fmap (toTuple . pure) $ Dynamic.mopcsDep (tupleArity @b) name rates =<< lift (fromTuple a)

liftOpr1kDep :: (Val a, Val b) => Name -> a -> SE b
liftOpr1kDep name b1 = SE $ fmap (fromE . pure) $ Dynamic.opr1kDep name =<< lift (toE b1)
