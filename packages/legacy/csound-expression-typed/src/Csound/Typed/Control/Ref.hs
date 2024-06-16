{-# Language ScopedTypeVariables, FlexibleContexts #-}
module Csound.Typed.Control.Ref(
    Ref(..), writeRef, readRef, newRef, mixRef, modifyRef, sensorsSE, newGlobalRef,
    concatRef, concatRef3, concatRef4, concatRef5,
    newCtrlRef, newGlobalCtrlRef,
    globalSensorsSE, newClearableGlobalRef, newTab, newGlobalTab,
    -- conditionals
    whileRef, whileRefD
) where

import Data.Boolean
import Data.Proxy

import Control.Monad
import Control.Monad.Trans.Class
import Csound.Dynamic hiding (when1, newLocalVars, writeArr, readArr, toCtrlRate)

import Csound.Typed.Types.Prim
import Csound.Typed.Types.Tuple
import Csound.Typed.GlobalState.SE
import Csound.Typed.GlobalState.GE

import qualified Csound.Dynamic as D

-- | It describes a reference to mutable values.
newtype Ref a = Ref [Var]
{-
    { writeRef :: a -> SE ()
    , readRef  :: SE a }
-}

writeRef :: Tuple a => Ref a -> a -> SE ()
writeRef (Ref vars) a = fromDep_ $ hideGEinDep $ do
    vals <- fromTuple a
    return $ zipWithM_ (writeVar IfKr) vars vals

--    (zipWithM_ writeVar vars) =<< lift (fromTuple a)
--writeVar :: Var -> E -> Dep ()
--[Var] (GE [E])

readRef  :: Tuple a => Ref a -> SE a
readRef (Ref vars) = SE $ fmap (toTuple . return) $ mapM (readVar IfKr) vars

-- | Allocates a new local (it is visible within the instrument) mutable value and initializes it with value.
-- A reference can contain a tuple of variables.
newRef :: forall a. Tuple a => a -> SE (Ref a)
newRef t = fmap Ref $ newLocalVars (tupleRates (Proxy :: Proxy a)) (fromTuple t)

-- | Allocates a new local (it is visible within the instrument) mutable value and initializes it with value.
-- A reference can contain a tuple of variables.
-- It contains control signals (k-rate) and constants for numbers (i-rates).
newCtrlRef :: forall a. Tuple a => a -> SE (Ref a)
newCtrlRef t = fmap Ref $ newLocalVars (fmap toCtrlRate $ tupleRates (Proxy :: Proxy a)) (fromTuple t)

toCtrlRate :: Rate -> Rate
toCtrlRate x = case x of
    Ar -> Kr
    _  -> x

concatRef :: (Tuple a, Tuple b) => Ref a -> Ref b -> Ref (a, b)
concatRef (Ref a) (Ref b) = Ref (a ++ b)

concatRef3 :: (Tuple a, Tuple b, Tuple c) => Ref a -> Ref b -> Ref c -> Ref (a, b, c)
concatRef3 (Ref a) (Ref b) (Ref c) = Ref (a ++ b ++ c)

concatRef4 :: (Tuple a, Tuple b, Tuple c, Tuple d) => Ref a -> Ref b -> Ref c -> Ref d -> Ref (a, b, c, d)
concatRef4 (Ref a) (Ref b) (Ref c) (Ref d) = Ref (a ++ b ++ c ++ d)

concatRef5 :: (Tuple a, Tuple b, Tuple c, Tuple d, Tuple e) => Ref a -> Ref b -> Ref c -> Ref d -> Ref e -> Ref (a, b, c, d, e)
concatRef5 (Ref a) (Ref b) (Ref c) (Ref d) (Ref e) = Ref (a ++ b ++ c ++ d ++ e)


-- | Adds the given signal to the value that is contained in the
-- reference.
mixRef :: (Num a, Tuple a) => Ref a -> a -> SE ()
mixRef ref asig = modifyRef ref (+ asig)

-- | Modifies the Ref value with given function.
modifyRef :: Tuple a => Ref a -> (a -> a) -> SE ()
modifyRef ref f = do
    v <- readRef ref
    writeRef ref (f v)

-- | An alias for the function @newRef@. It returns not the reference
-- to mutable value but a pair of reader and writer functions.
sensorsSE :: Tuple a => a -> SE (SE a, a -> SE ())
sensorsSE a = do
    ref <- newCtrlRef a
    return $ (readRef ref, writeRef ref)

-- | Allocates a new global mutable value and initializes it with value.
-- A reference can contain a tuple of variables.
newGlobalRef :: forall a. Tuple a => a -> SE (Ref a)
newGlobalRef t = fmap Ref $ newGlobalVars (tupleRates (Proxy :: Proxy a)) (fromTuple t)

-- | Allocates a new global mutable value and initializes it with value.
-- A reference can contain a tuple of variables.
-- It contains control signals (k-rate) and constants for numbers (i-rates).
newGlobalCtrlRef :: forall a . Tuple a => a -> SE (Ref a)
newGlobalCtrlRef t = fmap Ref $ newGlobalVars (fmap toCtrlRate $ tupleRates (Proxy :: Proxy a)) (fromTuple t)

-- | An alias for the function @newRef@. It returns not the reference
-- to mutable value but a pair of reader and writer functions.
globalSensorsSE :: Tuple a => a -> SE (SE a, a -> SE ())
globalSensorsSE a = do
    ref <- newGlobalRef a
    return $ (readRef ref, writeRef ref)

-- | Allocates a new clearable global mutable value and initializes it with value.
-- A reference can contain a tuple of variables.
-- The variable is set to zero at the end of every iteration.
-- It's useful for accumulation of audio values from several instruments.
newClearableGlobalRef :: forall a . Tuple a => a -> SE (Ref a)
newClearableGlobalRef t = fmap Ref $ newClearableGlobalVars (tupleRates (Proxy :: Proxy a)) (fromTuple t)

-------------------------------------------------------------------------------
-- writable tables

-- | Creates a new table. The Tab could be used while the instrument
-- is playing. When the instrument is retriggered the new tab is allocated.
--
-- > newTab size
newTab :: D -> SE Tab
newTab size = ftgentmp 0 0 size 7 0 [size, 0]

-- | Creates a new global table.
-- It's generated only once. It's persisted between instrument calls.
--
-- > newGlobalTab identifier size
newGlobalTab :: Int -> SE Tab
newGlobalTab size = do
    ref <- newGlobalCtrlRef ((fromGE $ saveWriteTab size) :: D)
    fmap (fromGE . toGE) $ readRef ref

-----------------------------------------------------------------------
-- some opcodes that I have to define upfront

-- |
-- Generate a score function table from within the orchestra, which is deleted at the end of the note.
--
-- Generate a score function table from within the orchestra,
--     which is optionally deleted at the end of the note.
--
-- > ifno  ftgentmp  ip1, ip2dummy, isize, igen, iarga, iargb, ...
--
-- csound doc: <http://www.csounds.com/manual/html/ftgentmp.html>
ftgentmp ::  D -> D -> D -> D -> D -> [D] -> SE Tab
ftgentmp b1 b2 b3 b4 b5 b6 =
  fmap ( Tab . return) $
    SE $ join $ f <$> unD' b1 <*> unD' b2 <*> unD' b3 <*> unD' b4 <*> unD' b5 <*> mapM unD' b6
    where
      f a1 a2 a3 a4 a5 a6 = opcsDep "ftgentmp" [(Ir,(repeat Ir))] ([a1,a2,a3,a4,a5] ++ a6)
      unD' = lift . unD

--------------------------------------------------------------------

whileRef :: forall st . Tuple st => st -> (st -> BoolSig) -> (st -> SE st) -> SE ()
whileRef initVal c body = do
    refSt   <- newCtrlRef initVal
    whileRefBy IfKr c refSt $ do
      writeRef refSt   =<< body    =<< readRef refSt

whileRefD :: forall st . Tuple st => st -> (st -> BoolD) -> (st -> SE st) -> SE ()
whileRefD initVal c body = do
    refSt   <- newCtrlRef initVal
    whileRefBy IfIr c refSt $ do
      writeRef refSt   =<< body    =<< readRef refSt

whileRefBy :: (Val bool, Tuple a) => IfRate -> (a -> bool) -> Ref a -> SE () -> SE ()
whileRefBy ifRate check (Ref vars) = ifBlockBy (D.whileBlock ifRate) (check $ toTuple $ pure $ fmap (D.inlineVar ifRate) vars)

-- | Constructs generic if-block statement with single then case
-- We can choose constructors for: if, while, until statements
ifBlockBy :: Val cond => (E -> DepT GE () -> DepT GE ()) -> cond -> SE () -> SE ()
ifBlockBy cons p body =
  fromDep_ $ do
    pE <- lift $ toGE p
    cons pE (unSE body)


