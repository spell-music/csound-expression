module Csound.Typed.Opcode.TableControl (
    
    
    
    ftfree, ftgen, ftgentmp, getftargs, sndload) where

import Control.Monad.Trans.Class
import Control.Monad
import Csound.Dynamic
import Csound.Typed

-- 

-- | 
-- Deletes function table.
--
-- >  ftfree  ifno, iwhen
--
-- csound doc: <http://csound.com/docs/manual/ftfree.html>
ftfree ::  Tab -> D -> SE ()
ftfree b1 b2 =
  SE $ join $ f <$> (lift . unTab) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "ftfree" [(Xr,[Ir,Ir])] [a1,a2]

-- | 
-- Generate a score function table from within the orchestra.
--
-- > gir  ftgen  ifn, itime, isize, igen, iarga [, iargb ] [...]
-- > gir  ftgen  ifn, itime, isize, igen, iarray
--
-- csound doc: <http://csound.com/docs/manual/ftgen.html>
ftgen ::  Tab -> D -> D -> D -> D -> SE D
ftgen b1 b2 b3 b4 b5 =
  fmap ( D . return) $ SE $ join $ f <$> (lift . unTab) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep "ftgen" [(Ir,(repeat Ir)),(Ir,[Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3,a4,a5]

-- | 
-- Generate a score function table from within the orchestra, which is deleted at the end of the note.
--
-- Generate a score function table from within the orchestra,
--     which is optionally deleted at the end of the note.
--
-- > ifno  ftgentmp  ip1, ip2dummy, isize, igen, iarga, iargb, ...
--
-- csound doc: <http://csound.com/docs/manual/ftgentmp.html>
ftgentmp ::  D -> D -> D -> D -> D -> [D] -> SE Tab
ftgentmp b1 b2 b3 b4 b5 b6 =
  fmap ( Tab . return) $ SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> mapM (lift . unD) b6
  where
    f a1 a2 a3 a4 a5 a6 = opcsDep "ftgentmp" [(Ir,(repeat Ir))] ([a1,a2,a3,a4,a5] ++ a6)

-- | 
-- Fill a string variable with the arguments used to create a function table at k-rate.
--
-- getftargs writes the arguments used to create a function table to a string variable. getftargs runs both at initialization and performance time.
--
-- > Sdst  getftargs  iftno, ktrig
--
-- csound doc: <http://csound.com/docs/manual/getftargs.html>
getftargs ::  D -> Sig -> Str
getftargs b1 b2 =
  Str $ f <$> unD b1 <*> unSig b2
  where
    f a1 a2 = opcs "getftargs" [(Sr,[Ir,Kr])] [a1,a2]

-- | 
-- Loads a sound file into memory for use by loscilx
--
-- sndload loads a sound file into memory for use by loscilx.
--
-- >  sndload  Sfname[, ifmt[, ichns[, isr[, ibas[, iamp[, istrt   \
-- >           [, ilpmod[, ilps[, ilpe]]]]]]]]]
--
-- csound doc: <http://csound.com/docs/manual/sndload.html>
sndload ::  Str -> SE ()
sndload b1 =
  SE $ join $ f <$> (lift . unStr) b1
  where
    f a1 = opcsDep_ "sndload" [(Xr,[Sr,Ir,Ir,Ir,Ir,Ir,Ir,Ir,Ir,Ir])] [a1]