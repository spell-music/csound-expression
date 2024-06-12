module Csound.Typed.Opcode.PluginHosting (
    
    
    -- * DSSI and LADSPA.
    dssiactivate, dssiaudio, dssictls, dssiinit, dssilist,
    
    -- * VST.
    vstaudio, vstaudiog, vstbankload, vstedit, vstinfo, vstinit, vstmidiout, vstnote, vstparamset, vstparamget, vstprogset) where

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
-- csound doc: <http://csound.com/docs/manual/dssiactivate.html>
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
-- csound doc: <http://csound.com/docs/manual/dssiaudio.html>
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
-- csound doc: <http://csound.com/docs/manual/dssictls.html>
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
-- csound doc: <http://csound.com/docs/manual/dssiinit.html>
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
-- csound doc: <http://csound.com/docs/manual/dssilist.html>
dssilist ::   SE ()
dssilist  =
  SE $ join $ return $ f 
  where
    f  = opcsDep_ "dssilist" [(Xr,[])] []

-- VST.

-- | 
-- VST audio output.
--
-- vstaudio and vstaudiog
--       are used for sending and receiving audio from a VST plugin.
--
-- > aout1,aout2  vstaudio  instance, [ain1, ain2]
--
-- csound doc: <http://csound.com/docs/manual/vstaudio.html>
vstaudio ::  D -> (Sig,Sig)
vstaudio b1 =
  pureTuple $ f <$> unD b1
  where
    f a1 = mopcs "vstaudio" ([Ar,Ar],[Ir,Ar,Ar]) [a1]

-- | 
-- VST audio output.
--
-- vstaudio and vstaudiog
--       are used for sending and receiving audio from a VST plugin.
--
-- > aout1,aout2  vstaudiog  instance, [ain1, ain2]
--
-- csound doc: <http://csound.com/docs/manual/vstaudio.html>
vstaudiog ::  D -> (Sig,Sig)
vstaudiog b1 =
  pureTuple $ f <$> unD b1
  where
    f a1 = mopcs "vstaudiog" ([Ar,Ar],[Ir,Ar,Ar]) [a1]

-- | 
-- Loads parameter banks to a VST plugin.
--
-- vstbankload is used for loading parameter
--       banks to a VST plugin.
--
-- >  vstbankload  instance, ipath
--
-- csound doc: <http://csound.com/docs/manual/vstbankload.html>
vstbankload ::  D -> D -> SE ()
vstbankload b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "vstbankload" [(Xr,[Ir,Ir])] [a1,a2]

-- | 
-- Opens the GUI editor window for a VST plugin.
--
-- vstedit opens the custom GUI editor window for a VST
--       plugin. Note that not all VST plugins have custom GUI editors. It may
--       be necessary to use the --displays command-line option to ensure that
--       Csound handles events from the editor window and displays it properly.
--
-- >  vstedit  instance
--
-- csound doc: <http://csound.com/docs/manual/vstedit.html>
vstedit ::  D -> SE ()
vstedit b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "vstedit" [(Xr,[Ir])] [a1]

-- | 
-- Displays the parameters and the programs of a VST plugin.
--
-- vstinfo displays the parameters and the programs of a
--       VST plugin.
--
-- >  vstinfo  instance
--
-- csound doc: <http://csound.com/docs/manual/vstinfo.html>
vstinfo ::  D -> SE ()
vstinfo b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "vstinfo" [(Xr,[Ir])] [a1]

-- | 
-- Load a VST plugin into memory for use with the other vst4cs opcodes.
--
-- vstinit is used to load a VST plugin into memory for use with
--       the other vst4cs opcodes. Both VST effects and instruments
--       (synthesizers) can be used.
--
-- > instance  vstinit  ilibrarypath [,iverbose]
--
-- csound doc: <http://csound.com/docs/manual/vstinit.html>
vstinit ::  D -> SE D
vstinit b1 =
  fmap ( D . return) $ SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep "vstinit" [(Ir,[Ir,Ir])] [a1]

-- | 
-- Sends MIDI information to a VST plugin.
--
-- vstmidiout is used for sending MIDI information to a VST plugin.
--
-- >  vstmidiout  instance, kstatus, kchan, kdata1, kdata2
--
-- csound doc: <http://csound.com/docs/manual/vstmidiout.html>
vstmidiout ::  D -> Sig -> Sig -> Sig -> Sig -> SE ()
vstmidiout b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "vstmidiout" [(Xr,[Ir,Kr,Kr,Kr,Kr])] [a1,a2,a3,a4,a5]

-- | 
-- Sends a MIDI note with definite duration to a VST plugin.
--
-- vstnote sends a MIDI note with definite duration to a VST plugin.
--
-- >  vstnote  instance, kchan, knote, kveloc, kdur
-- >         
--
-- csound doc: <http://csound.com/docs/manual/vstnote.html>
vstnote ::  D -> Sig -> Sig -> Sig -> Sig -> SE ()
vstnote b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "vstnote" [(Xr,[Ir,Kr,Kr,Kr,Kr])] [a1,a2,a3,a4,a5]

-- | 
-- Used for parameter comunication to and from a VST plugin.
--
-- vstparamset and vstparamget are used for parameter comunication to and from a VST plugin.
--
-- >  vstparamset instance, kparam, kvalue
--
-- csound doc: <http://csound.com/docs/manual/vstparamset.html>
vstparamset ::  D -> Sig -> Sig -> SE ()
vstparamset b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep_ "vstparamset" [(Xr,[Ir,Kr,Kr])] [a1,a2,a3]

-- | 
-- Used for parameter comunication to and from a VST plugin.
--
-- vstparamset and vstparamget are used for parameter comunication to and from a VST plugin.
--
-- > kvalue  vstparamget instance, kparam
--
-- csound doc: <http://csound.com/docs/manual/vstparamset.html>
vstparamget ::  D -> Sig -> Sig
vstparamget b1 b2 =
  Sig $ f <$> unD b1 <*> unSig b2
  where
    f a1 a2 = opcs "vstparamget" [(Kr,[Ir,Kr])] [a1,a2]

-- | 
-- Loads parameter banks to a VST plugin.
--
-- vstprogset sets one of the programs in an
--       .fxb bank.
--
-- >  vstprogset  instance, kprogram
--
-- csound doc: <http://csound.com/docs/manual/vstprogset.html>
vstprogset ::  D -> Sig -> SE ()
vstprogset b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "vstprogset" [(Xr,[Ir,Kr])] [a1,a2]