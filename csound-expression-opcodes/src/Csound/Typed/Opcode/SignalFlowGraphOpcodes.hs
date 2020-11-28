module Csound.Typed.Opcode.SignalFlowGraphOpcodes (
    
    
    
    ftgenonce, inleta, inletf, inletk, inletkid, inletv, outleta, outletf, outletk, outletkid, outletv) where

import Control.Applicative
import Control.Monad.Trans.Class
import Csound.Dynamic
import Csound.Typed

-- 

-- | 
-- Generate a function table from within an instrument definition, without duplication of data.
--
-- Enables the creation of function tables entirely inside 
--       instrument definitions, without any duplication of data.
--
-- > ifno  ftgenonce  ip1, ip2dummy, isize, igen, iarga, iargb, ...
--
-- csound doc: <http://csound.com/docs/manual/ftgenonce.html>
ftgenonce ::  D -> D -> D -> D -> D -> [D] -> SE Tab
ftgenonce b1 b2 b3 b4 b5 b6 = fmap ( Tab . return) $ SE $ (depT =<<) $ lift $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> mapM unD b6
    where f a1 a2 a3 a4 a5 a6 = opcs "ftgenonce" [(Ir,(repeat Ir))] ([a1,a2,a3,a4,a5] ++ a6)

-- | 
-- Receives an arate signal into an instrument through a named port.
--
-- > asignal  inleta Sname
--
-- csound doc: <http://csound.com/docs/manual/inleta.html>
inleta ::  Str -> Sig
inleta b1 = Sig $ f <$> unStr b1
    where f a1 = opcs "inleta" [(Ar,[Sr])] [a1]

-- | 
-- Receives an frate signal (fsig) into an instrument from a named port.
--
-- > fsignal  inletf Sname
--
-- csound doc: <http://csound.com/docs/manual/inletf.html>
inletf ::  Str -> Spec
inletf b1 = Spec $ f <$> unStr b1
    where f a1 = opcs "inletf" [(Fr,[Sr])] [a1]

-- | 
-- Receives a krate signal into an instrument from a named port.
--
-- > ksignal  inletk Sname
--
-- csound doc: <http://csound.com/docs/manual/inletk.html>
inletk ::  Str -> Sig
inletk b1 = Sig $ f <$> unStr b1
    where f a1 = opcs "inletk" [(Kr,[Sr])] [a1]

-- | 
-- Receives a krate signal into an instrument from a named port.
--
-- > ksignal  inletkid Sname, SinstanceID
--
-- csound doc: <http://csound.com/docs/manual/inletkid.html>
inletkid ::  Str -> Str -> Sig
inletkid b1 b2 = Sig $ f <$> unStr b1 <*> unStr b2
    where f a1 a2 = opcs "inletkid" [(Kr,[Sr,Sr])] [a1,a2]

-- | 
-- Receives an arate array signal into an instrument through a named port.
--
-- > array  inletv Sname
--
-- csound doc: <http://csound.com/docs/manual/inletv.html>
inletv ::  Str -> Sig
inletv b1 = Sig $ f <$> unStr b1
    where f a1 = opcs "inletv" [(Ar,[Sr])] [a1]

-- | 
-- Sends an arate signal out from an instrument to a named port.
--
-- >  outleta Sname, asignal
--
-- csound doc: <http://csound.com/docs/manual/outleta.html>
outleta ::  Str -> Sig -> SE ()
outleta b1 b2 = SE $ (depT_ =<<) $ lift $ f <$> unStr b1 <*> unSig b2
    where f a1 a2 = opcs "outleta" [(Xr,[Sr,Ar])] [a1,a2]

-- | 
-- Sends a frate signal (fsig) out from an instrument to a named port.
--
-- >  outletf Sname, fsignal
--
-- csound doc: <http://csound.com/docs/manual/outletf.html>
outletf ::  Str -> Spec -> SE ()
outletf b1 b2 = SE $ (depT_ =<<) $ lift $ f <$> unStr b1 <*> unSpec b2
    where f a1 a2 = opcs "outletf" [(Xr,[Sr,Fr])] [a1,a2]

-- | 
-- Sends a krate signal out from an instrument to a named port.
--
-- >  outletk Sname, ksignal
--
-- csound doc: <http://csound.com/docs/manual/outletk.html>
outletk ::  Str -> Sig -> SE ()
outletk b1 b2 = SE $ (depT_ =<<) $ lift $ f <$> unStr b1 <*> unSig b2
    where f a1 a2 = opcs "outletk" [(Xr,[Sr,Kr])] [a1,a2]

-- | 
-- Sends a krate signal out from an instrument to a named port.
--
-- >  outletkid Sname, SinstanceID, ksignal
--
-- csound doc: <http://csound.com/docs/manual/outletkid.html>
outletkid ::  Str -> Str -> Sig -> SE ()
outletkid b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unStr b1 <*> unStr b2 <*> unSig b3
    where f a1 a2 a3 = opcs "outletkid" [(Xr,[Sr,Sr,Kr])] [a1,a2,a3]

-- | 
-- Sends an arate array signal out from an instrument to a named port.
--
-- >  outletv Sname, array
--
-- csound doc: <http://csound.com/docs/manual/outletv.html>
outletv ::  Str -> Sig -> SE ()
outletv b1 b2 = SE $ (depT_ =<<) $ lift $ f <$> unStr b1 <*> unSig b2
    where f a1 a2 = opcs "outletv" [(Xr,[Sr,Ar])] [a1,a2]