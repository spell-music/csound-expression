module Csound.Typed.Opcode.PluginHosting (
    
    
    -- * DSSI and LADSPA.
    dssiactivate, dssiaudio, dssictls, dssiinit, dssilist) where

import Control.Monad.Trans.Class
import Control.Monad
import Csound.Dynamic
import Csound.Typed

-- DSSI and LADSPA.

-- | 
-- Activates or deactivates a DSSI or LADSPA plugin.
--
-- dssiactivate is used to activate or deactivate a DSSI or LADSPA plugin. It calles the plugin's activate() and deactivate() functions if they are provided.
--
-- >  dssiactivate  ihandle, ktoggle 
--
-- csound doc: <https://csound.com/docs/manual/dssiactivate.html>
dssiactivate ::  D -> Sig -> SE ()
dssiactivate b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "dssiactivate" [(Xr,[Ir,Kr])] [a1,a2]

-- | 
-- Processes audio using a LADSPA or DSSI plugin.
--
-- dssiaudio generates audio by processing an input signal through a LADSPA plugin.
--
-- > [aout1, aout2, ..., aout9]  dssiaudio  ihandle, [ain1, ain2, ..., ain9]
--
-- csound doc: <https://csound.com/docs/manual/dssiaudio.html>
dssiaudio :: forall a . Tuple a => D -> [Sig] -> a
dssiaudio b1 b2 =
  pureTuple $ f <$> unD b1 <*> mapM unSig b2
  where
    f a1 a2 = mopcs "dssiaudio" ((repeat Ar),[Ir] ++ (repeat Ar)) ([a1] ++ a2)

-- | 
-- Send control information to a LADSPA or DSSI plugin.
--
-- dssictls sends control values to a plugin's control port
--
-- >  dssictls  ihandle, iport, kvalue, ktrigger 
--
-- csound doc: <https://csound.com/docs/manual/dssictls.html>
dssictls ::  D -> D -> Sig -> Sig -> SE ()
dssictls b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "dssictls" [(Xr,[Ir,Ir,Kr,Kr])] [a1,a2,a3,a4]

-- | 
-- Loads a DSSI or LADSPA plugin.
--
-- dssiinit is used to load a DSSI or LADSPA plugin into memory for use with
--       the other dssi4cs opcodes. Both LADSPA effects and DSSI instruments can be used.
--
-- > ihandle  dssiinit  ilibraryname, iplugindex [, iverbose] 
--
-- csound doc: <https://csound.com/docs/manual/dssiinit.html>
dssiinit ::  D -> D -> SE D
dssiinit b1 b2 =
  fmap ( D . return) $ SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep "dssiinit" [(Ir,[Ir,Ir,Ir])] [a1,a2]

-- | 
-- Lists all available DSSI and LADSPA plugins.
--
-- dssilist checks the variables DSSI_PATH and LADSPA_PATH and lists all plugins available in all plugin libraries there.
--
-- >  dssilist 
--
-- csound doc: <https://csound.com/docs/manual/dssilist.html>
dssilist ::   SE ()
dssilist  =
  SE $ join $ return $ f 
  where
    f  = opcsDep_ "dssilist" [(Xr,[])] []