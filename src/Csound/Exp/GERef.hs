module Csound.Exp.GERef(
    GERef, newGERef, readGERef, writeGERef, 
    sensorsGE
) where

import Control.Monad(zipWithM, zipWithM_)

import Csound.Exp.Tuple
import Csound.Exp.SE
import Csound.Exp.GE

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

