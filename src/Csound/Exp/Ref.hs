module Csound.Exp.Ref(
    -- * GERef
    GERef, newGERef, readGERef, writeGERef, 
    sensorsGE, appendGERef, appendGERefBy,
    readOnlyRef, appendRef, mkSink, mkAppendSink,

    -- * SERef
    SERef, newSERef, readSERef, writeSERef, 
    sensorsSE
) where

import Control.Monad(zipWithM, zipWithM_)
import Data.Monoid

import Csound.Exp(E, Var)
import Csound.Exp.Wrapper
import Csound.Exp.Tuple
import Csound.Exp.GE
import Csound.Exp.SE

-- global references

data GERef a = GERef 
    { readGERef  :: SE a
    , writeGERef :: a -> SE () }

sensorsGE :: CsdTuple a => a -> GE (SE a, a -> SE ())
sensorsGE a = do
    vs <- zipWithM newGlobalVar (ratesCsdTuple a) (fromCsdTuple a)
    let reader = fmap toCsdTuple $ mapM readVar vs
        writer x = zipWithM_ writeVar vs (fromCsdTuple x)
    return (reader, writer)

newGERef :: CsdTuple a => a -> GE (GERef a)
newGERef a = fmap (uncurry GERef) $ sensorsGE a

appendGERef :: Monoid a => GERef a -> a -> SE ()
appendGERef = appendGERefBy mappend

appendGERefBy :: (a -> a -> a) -> GERef a -> a -> SE ()
appendGERefBy op ref x = do
    cur <- readGERef ref
    writeGERef ref $ op cur x

-- global read-only-write-once references (writer is hidden from the user)
  
mkSink, mkAppendSink :: (Out a) => a -> GE (NoSE a, E)

mkSink          = genMkSink readOnlyRef
mkAppendSink    = genMkSink appendRef

genMkSink :: (Out a) => GE (NoSE a, NoSE a -> SE ()) -> a -> GE (NoSE a, E)
genMkSink ref a = do
    (reader, writer) <- ref
    return (reader, execSE $ writer . toCsdTuple . fmap toE =<< toOut a)

readOnlyRef, appendRef :: (CsdTuple a) => GE (a, a -> SE ())

readOnlyRef = genReadOnlyRef writeVar
appendRef   = genReadOnlyRef $ appendVarBy (+)

genReadOnlyRef :: (CsdTuple a) => (Var -> E -> SE ()) -> GE (a, a -> SE ())
genReadOnlyRef writeRef = res
    where 
        a = proxy defCsdTuple res
        res = do         
            vs <- zipWithM newGlobalVar (ratesCsdTuple a) (fromCsdTuple a)
            let reader = toCsdTuple $ fmap readOnlyVar vs
                writer x = zipWithM_ writeRef vs (fromCsdTuple x)
            return (reader, writer)
        proxy :: a -> GE (a, a -> SE ()) -> a
        proxy = const

-- local references

data SERef a = SERef
    { readSERef  :: SE a
    , writeSERef :: a -> SE () }

newSERef :: CsdTuple a => a -> SE (SERef a)
newSERef a = fmap (uncurry SERef) (sensorsSE a)

sensorsSE :: CsdTuple a => a -> SE (SE a, a -> SE ())
sensorsSE a = do
    vs <- zipWithM newLocalVar (ratesCsdTuple a) (fromCsdTuple a)
    let reader = fmap toCsdTuple $ mapM readVar vs
        writer x = zipWithM_ writeVar vs (fromCsdTuple x)
    return (reader, writer)
    


