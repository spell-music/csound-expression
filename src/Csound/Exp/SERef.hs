module Csound.Exp.SERef(
    SERef, newSERef, readSERef, writeSERef, 
    sensorsSE
) where

import Control.Monad(zipWithM, zipWithM_)

import Csound.Exp.Tuple
import Csound.Exp.SE

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
    


