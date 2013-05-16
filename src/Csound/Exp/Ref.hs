module Csound.Exp.Ref(
    -- * GERef
    GERef, newGERef, readGERef, writeGERef, 
    sensorsGE, 

    -- * SERef
    SERef, newSERef, readSERef, writeSERef, 
    sensorsSE
) where

import Control.Monad(zipWithM, zipWithM_)

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
    let reader = return $ toCsdTuple $ fmap readVar vs
        writer x = zipWithM_ writeVar vs (fromCsdTuple x)
    return (reader, writer)

newGERef :: CsdTuple a => a -> GE (GERef a)
newGERef a = fmap (uncurry GERef) $ sensorsGE a

-- local references

data SERef a = SERef
    { readSERef  :: SE a
    , writeSERef :: a -> SE () }

newSERef :: CsdTuple a => a -> SE (SERef a)
newSERef a = fmap (uncurry SERef) (sensorsSE a)

sensorsSE :: CsdTuple a => a -> SE (SE a, a -> SE ())
sensorsSE a = do
    vs <- zipWithM newLocalVar (ratesCsdTuple a) (fromCsdTuple a)
    let reader = return $ toCsdTuple $ fmap readVar vs
        writer x = zipWithM_ writeVar vs (fromCsdTuple x)
    return (reader, writer)
    


