module Csound.Typed.Opcode.RealtimeMIDI (
    
    
    -- * Input.
    aftouch, chanctrl, ctrl14, ctrl21, ctrl7, ctrlinit, ctrlpreset, ctrlprint, ctrlprintpresets, ctrlsave, ctrlselect, initc14, initc21, initc7, massign, midic14, midic21, midic7, midictrl, notnum, pchbend, pgmassign, polyaft, veloc,
    
    -- * Output.
    nrpn, outiat, outic, outic14, outipat, outipb, outipc, outkat, outkc, outkc14, outkpat, outkpb, outkpc,
    
    -- * Converters.
    ampmidi, ampmidicurve, ampmidid, cpsmidi, cpsmidib, cpstmid, octmidi, octmidib, pchmidi, pchmidib,
    
    -- * Generic I/O.
    midiin, midiout, midiout_i,
    
    -- * Event Extenders.
    lastcycle, release, xtratim,
    
    -- * Note Output.
    midiarp, midion, midion2, moscil, noteoff, noteon, noteondur, noteondur2,
    
    -- * MIDI/Score Interoperability.
    midichannelaftertouch, midichn, midicontrolchange, mididefault, midinoteoff, midinoteoncps, midinoteonkey, midinoteonoct, midinoteonpch, midipitchbend, midipolyaftertouch, midiprogramchange,
    
    -- * System Realtime.
    mclock, mrtmsg) where

import Control.Monad.Trans.Class
import Control.Monad
import Csound.Dynamic
import Csound.Typed

-- Input.

-- | 
-- Get the current after-touch value for this channel.
--
-- > kaft  aftouch  [imin] [, imax]
--
-- csound doc: <https://csound.com/docs/manual/aftouch.html>
aftouch ::   Sig
aftouch  =
  Sig $ return $ f 
  where
    f  = opcs "aftouch" [(Kr,[Ir,Ir])] []

-- | 
-- Get the current value of a MIDI channel controller.
--
-- Get the current value of a controller and optionally map it onto specified range.
--
-- > ival  chanctrl  ichnl, ictlno [, ilow] [, ihigh]
-- > kval  chanctrl  ichnl, ictlno [, ilow] [, ihigh]
--
-- csound doc: <https://csound.com/docs/manual/chanctrl.html>
chanctrl ::  D -> D -> Sig
chanctrl b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "chanctrl" [(Ir,[Ir,Ir,Ir,Ir]),(Kr,[Ir,Ir,Ir,Ir])] [a1,a2]

-- | 
-- Allows a floating-point 14-bit MIDI signal scaled with a minimum and a maximum range.
--
-- > idest  ctrl14  ichan, ictlno1, ictlno2, imin, imax [, ifn]
-- > kdest  ctrl14  ichan, ictlno1, ictlno2, kmin, kmax [, ifn]
--
-- csound doc: <https://csound.com/docs/manual/ctrl14.html>
ctrl14 ::  D -> D -> D -> D -> D -> Sig
ctrl14 b1 b2 b3 b4 b5 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5
  where
    f a1 a2 a3 a4 a5 = opcs "ctrl14" [(Ir,[Ir,Ir,Ir,Ir,Ir,Ir]),(Kr,[Ir,Ir,Ir,Kr,Kr,Ir])] [a1
                                                                                         ,a2
                                                                                         ,a3
                                                                                         ,a4
                                                                                         ,a5]

-- | 
-- Allows a floating-point 21-bit MIDI signal scaled with a minimum and a maximum range.
--
-- > idest  ctrl21  ichan, ictlno1, ictlno2, ictlno3, imin, imax [, ifn]
-- > kdest  ctrl21  ichan, ictlno1, ictlno2, ictlno3, kmin, kmax [, ifn]
--
-- csound doc: <https://csound.com/docs/manual/ctrl21.html>
ctrl21 ::  D -> D -> D -> D -> D -> D -> Sig
ctrl21 b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unD b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "ctrl21" [(Ir,[Ir,Ir,Ir,Ir,Ir,Ir,Ir])
                                        ,(Kr,[Ir,Ir,Ir,Ir,Kr,Kr,Ir])] [a1,a2,a3,a4,a5,a6]

-- | 
-- Allows a floating-point 7-bit MIDI signal scaled with a minimum and a maximum range.
--
-- > idest  ctrl7  ichan, ictlno, imin, imax [, ifn]
-- > kdest  ctrl7  ichan, ictlno, kmin, kmax [, ifn]
-- > adest  ctrl7  ichan, ictlno, kmin, kmax [, ifn] [, icutoff]
--
-- csound doc: <https://csound.com/docs/manual/ctrl7.html>
ctrl7 ::  D -> D -> D -> D -> Sig
ctrl7 b1 b2 b3 b4 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "ctrl7" [(Ir,[Ir,Ir,Ir,Ir,Ir])
                                 ,(Kr,[Ir,Ir,Kr,Kr,Ir])
                                 ,(Ar,[Ir,Ir,Kr,Kr,Ir,Ir])] [a1,a2,a3,a4]

-- | 
-- Sets the initial values for a set of MIDI controllers.
--
-- >  ctrlinit  ichnl, ictlno1, ival1 [, ictlno2] [, ival2] [, ictlno3] \
-- >           [, ival3] [,...ival32]
--
-- csound doc: <https://csound.com/docs/manual/ctrlinit.html>
ctrlinit ::  [D] -> SE ()
ctrlinit b1 =
  SE $ join $ f <$> mapM (lift . unD) b1
  where
    f a1 = opcsDep_ "ctrlinit" [(Xr,(repeat Ir))] a1

-- | 

--
-- > kpreset  ctrlpreset  ktag, kchnl, kctlno1, [kctlno2] [, kctlno3] ...
--
-- csound doc: <https://csound.com/docs/manual/ctrlpreset.html>
ctrlpreset ::  Sig -> Sig -> Sig -> Sig
ctrlpreset b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "ctrlpreset" [(Kr,(repeat Kr))] [a1,a2,a3]

-- | 

--
-- >   ctrlprint  kcont[][, Sfile]
--
-- csound doc: <https://csound.com/docs/manual/ctrlprint.html>
ctrlprint ::  Sig -> SE ()
ctrlprint b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "ctrlprint" [(Xr,[Kr,Sr])] [a1]

-- | 

--
-- >   ctrlprintpresets  [Sfilenam]
--
-- csound doc: <https://csound.com/docs/manual/ctrlprintpresets.html>
ctrlprintpresets ::   SE ()
ctrlprintpresets  =
  SE $ join $ return $ f 
  where
    f  = opcsDep_ "ctrlprintpresets" [(Xr,[Sr])] []

-- | 

--
-- > kconnt[]  ctrlsave  ichnl, ictlno1, [ictlno2] [, ictlno3] ...
--
-- csound doc: <https://csound.com/docs/manual/ctrlsave.html>
ctrlsave ::  D -> D -> Sig
ctrlsave b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "ctrlsave" [(Kr,(repeat Ir))] [a1,a2]

-- | 

--
-- >   ctrlselect  kpre
--
-- csound doc: <https://csound.com/docs/manual/ctrlselect.html>
ctrlselect ::  Sig -> SE ()
ctrlselect b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "ctrlselect" [(Xr,[Kr])] [a1]

-- | 
-- Initializes the controllers used to create a 14-bit MIDI value.
--
-- >  initc14  ichan, ictlno1, ictlno2, ivalue
--
-- csound doc: <https://csound.com/docs/manual/initc14.html>
initc14 ::  D -> D -> D -> D -> SE ()
initc14 b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "initc14" [(Xr,[Ir,Ir,Ir,Ir])] [a1,a2,a3,a4]

-- | 
-- Initializes the controllers used to create a 21-bit MIDI value.
--
-- >  initc21  ichan, ictlno1, ictlno2, ictlno3, ivalue
--
-- csound doc: <https://csound.com/docs/manual/initc21.html>
initc21 ::  D -> D -> D -> D -> D -> SE ()
initc21 b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "initc21" [(Xr,[Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3,a4,a5]

-- | 
-- Initializes the controller used to create a 7-bit MIDI value.
--
-- Initializes MIDI controller ictlno with ivalue
--
-- >  initc7  ichan, ictlno, ivalue
--
-- csound doc: <https://csound.com/docs/manual/initc7.html>
initc7 ::  D -> D -> D -> SE ()
initc7 b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "initc7" [(Xr,[Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Assigns a MIDI channel number to a Csound instrument.
--
-- >  massign  ichnl, insnum[, ireset]
-- >  massign  ichnl, "insname"[, ireset]
--
-- csound doc: <https://csound.com/docs/manual/massign.html>
massign ::  D -> D -> SE ()
massign b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "massign" [(Xr,[Ir,Ir,Ir])] [a1,a2]

-- | 
-- Allows a floating-point 14-bit MIDI signal scaled with a minimum and a maximum range.
--
-- > idest  midic14  ictlno1, ictlno2, imin, imax [, ifn]
-- > kdest  midic14  ictlno1, ictlno2, kmin, kmax [, ifn]
--
-- csound doc: <https://csound.com/docs/manual/midic14.html>
midic14 ::  D -> D -> D -> D -> Sig
midic14 b1 b2 b3 b4 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "midic14" [(Ir,[Ir,Ir,Ir,Ir,Ir]),(Kr,[Ir,Ir,Kr,Kr,Ir])] [a1,a2,a3,a4]

-- | 
-- Allows a floating-point 21-bit MIDI signal scaled with a minimum and a maximum range.
--
-- > idest  midic21  ictlno1, ictlno2, ictlno3, imin, imax [, ifn]
-- > kdest  midic21  ictlno1, ictlno2, ictlno3, kmin, kmax [, ifn]
--
-- csound doc: <https://csound.com/docs/manual/midic21.html>
midic21 ::  D -> D -> D -> D -> D -> Sig
midic21 b1 b2 b3 b4 b5 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5
  where
    f a1 a2 a3 a4 a5 = opcs "midic21" [(Ir,[Ir,Ir,Ir,Ir,Ir,Ir]),(Kr,[Ir,Ir,Ir,Kr,Kr,Ir])] [a1
                                                                                          ,a2
                                                                                          ,a3
                                                                                          ,a4
                                                                                          ,a5]

-- | 
-- Allows a floating-point 7-bit MIDI signal scaled with a minimum and a maximum range.
--
-- > idest  midic7  ictlno, imin, imax [, ifn]
-- > kdest  midic7  ictlno, kmin, kmax [, ifn]
--
-- csound doc: <https://csound.com/docs/manual/midic7.html>
midic7 ::  D -> D -> D -> Sig
midic7 b1 b2 b3 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "midic7" [(Ir,[Ir,Ir,Ir,Ir]),(Kr,[Ir,Kr,Kr,Ir])] [a1,a2,a3]

-- | 
-- Get the current value (0-127) of a specified MIDI controller.
--
-- > ival  midictrl  inum [, imin] [, imax]
-- > kval  midictrl  inum [, imin] [, imax]
--
-- csound doc: <https://csound.com/docs/manual/midictrl.html>
midictrl ::  D -> Sig
midictrl b1 =
  Sig $ f <$> unD b1
  where
    f a1 = opcs "midictrl" [(Ir,[Ir,Ir,Ir]),(Kr,[Ir,Ir,Ir])] [a1]

-- | 
-- Get a note number from a MIDI event.
--
-- > ival  notnum 
--
-- csound doc: <https://csound.com/docs/manual/notnum.html>
notnum ::  Msg -> D
notnum _  =
  D $ return $ f 
  where
    f  = opcs "notnum" [(Ir,[])] []

-- | 
-- Get the current pitch-bend value for this channel.
--
-- > ibend  pchbend  [imin] [, imax]
-- > kbend  pchbend  [imin] [, imax]
--
-- csound doc: <https://csound.com/docs/manual/pchbend.html>
pchbend ::  Msg -> Sig
pchbend _  =
  Sig $ return $ f 
  where
    f  = opcs "pchbend" [(Ir,[Ir,Ir]),(Kr,[Ir,Ir])] []

-- | 
-- Assigns an instrument number to a specified MIDI program.
--
-- Assigns an instrument number to a specified (or all) MIDI program(s).
--
-- >  pgmassign  ipgm, inst[, ichn]
-- >  pgmassign  ipgm, "insname"[, ichn]
--
-- csound doc: <https://csound.com/docs/manual/pgmassign.html>
pgmassign ::  D -> D -> SE ()
pgmassign b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "pgmassign" [(Xr,[Ir,Ir,Ir])] [a1,a2]

-- | 
-- Returns the polyphonic after-touch pressure of the selected note number.
--
-- polyaft returns the polyphonic pressure of the selected note number, optionally mapped to an user-specified range.
--
-- > ires  polyaft  inote [, ilow] [, ihigh]
-- > kres  polyaft  inote [, ilow] [, ihigh]
--
-- csound doc: <https://csound.com/docs/manual/polyaft.html>
polyaft ::  D -> Sig
polyaft b1 =
  Sig $ f <$> unD b1
  where
    f a1 = opcs "polyaft" [(Ir,[Ir,Ir,Ir]),(Kr,[Ir,Ir,Ir])] [a1]

-- | 
-- Get the velocity from a MIDI event.
--
-- > ival  veloc  [ilow] [, ihigh]
--
-- csound doc: <https://csound.com/docs/manual/veloc.html>
veloc ::  Msg -> D
veloc _  =
  D $ return $ f 
  where
    f  = opcs "veloc" [(Ir,[Ir,Ir])] []

-- Output.

-- | 
-- Sends a Non-Registered Parameter Number to the MIDI OUT port.
--
-- Sends a NPRN (Non-Registered Parameter Number) message to the MIDI OUT port each time one of the input arguments changes.
--
-- >  nrpn  kchan, kparmnum, kparmvalue
--
-- csound doc: <https://csound.com/docs/manual/nrpn.html>
nrpn ::  Sig -> Sig -> Sig -> SE ()
nrpn b1 b2 b3 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep_ "nrpn" [(Xr,[Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Sends MIDI aftertouch messages at i-rate.
--
-- >  outiat  ichn, ivalue, imin, imax
--
-- csound doc: <https://csound.com/docs/manual/outiat.html>
outiat ::  D -> D -> D -> D -> SE ()
outiat b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "outiat" [(Xr,[Ir,Ir,Ir,Ir])] [a1,a2,a3,a4]

-- | 
-- Sends MIDI controller output at i-rate.
--
-- >  outic  ichn, inum, ivalue, imin, imax
--
-- csound doc: <https://csound.com/docs/manual/outic.html>
outic ::  D -> D -> D -> D -> D -> SE ()
outic b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "outic" [(Xr,[Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3,a4,a5]

-- | 
-- Sends 14-bit MIDI controller output at i-rate.
--
-- >  outic14  ichn, imsb, ilsb, ivalue, imin, imax
--
-- csound doc: <https://csound.com/docs/manual/outic14.html>
outic14 ::  D -> D -> D -> D -> D -> D -> SE ()
outic14 b1 b2 b3 b4 b5 b6 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6
  where
    f a1 a2 a3 a4 a5 a6 = opcsDep_ "outic14" [(Xr,[Ir,Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3,a4,a5,a6]

-- | 
-- Sends polyphonic MIDI aftertouch messages at i-rate.
--
-- >  outipat  ichn, inotenum, ivalue, imin, imax
--
-- csound doc: <https://csound.com/docs/manual/outipat.html>
outipat ::  D -> D -> D -> D -> D -> SE ()
outipat b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "outipat" [(Xr,[Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3,a4,a5]

-- | 
-- Sends MIDI pitch-bend messages at i-rate.
--
-- >  outipb  ichn, ivalue, imin, imax
--
-- csound doc: <https://csound.com/docs/manual/outipb.html>
outipb ::  D -> D -> D -> D -> SE ()
outipb b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "outipb" [(Xr,[Ir,Ir,Ir,Ir])] [a1,a2,a3,a4]

-- | 
-- Sends MIDI program change messages at i-rate
--
-- >  outipc  ichn, iprog, imin, imax
--
-- csound doc: <https://csound.com/docs/manual/outipc.html>
outipc ::  D -> D -> D -> D -> SE ()
outipc b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "outipc" [(Xr,[Ir,Ir,Ir,Ir])] [a1,a2,a3,a4]

-- | 
-- Sends MIDI aftertouch messages at k-rate.
--
-- >  outkat  kchn, kvalue, kmin, kmax
--
-- csound doc: <https://csound.com/docs/manual/outkat.html>
outkat ::  Sig -> Sig -> Sig -> Sig -> SE ()
outkat b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "outkat" [(Xr,[Kr,Kr,Kr,Kr])] [a1,a2,a3,a4]

-- | 
-- Sends MIDI controller messages at k-rate.
--
-- >  outkc  kchn, knum, kvalue, kmin, kmax
--
-- csound doc: <https://csound.com/docs/manual/outkc.html>
outkc ::  Sig -> Sig -> Sig -> Sig -> Sig -> SE ()
outkc b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "outkc" [(Xr,[Kr,Kr,Kr,Kr,Kr])] [a1,a2,a3,a4,a5]

-- | 
-- Sends 14-bit MIDI controller output at k-rate.
--
-- >  outkc14  kchn, kmsb, klsb, kvalue, kmin, kmax
--
-- csound doc: <https://csound.com/docs/manual/outkc14.html>
outkc14 ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> SE ()
outkc14 b1 b2 b3 b4 b5 b6 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5 <*> (lift . unSig) b6
  where
    f a1 a2 a3 a4 a5 a6 = opcsDep_ "outkc14" [(Xr,[Kr,Kr,Kr,Kr,Kr,Kr])] [a1,a2,a3,a4,a5,a6]

-- | 
-- Sends polyphonic MIDI aftertouch messages at k-rate.
--
-- >  outkpat  kchn, knotenum, kvalue, kmin, kmax
--
-- csound doc: <https://csound.com/docs/manual/outkpat.html>
outkpat ::  Sig -> Sig -> Sig -> Sig -> Sig -> SE ()
outkpat b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "outkpat" [(Xr,[Kr,Kr,Kr,Kr,Kr])] [a1,a2,a3,a4,a5]

-- | 
-- Sends MIDI pitch-bend messages at k-rate.
--
-- >  outkpb  kchn, kvalue, kmin, kmax
--
-- csound doc: <https://csound.com/docs/manual/outkpb.html>
outkpb ::  Sig -> Sig -> Sig -> Sig -> SE ()
outkpb b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "outkpb" [(Xr,[Kr,Kr,Kr,Kr])] [a1,a2,a3,a4]

-- | 
-- Sends MIDI program change messages at k-rate.
--
-- >  outkpc  kchn, kprog, kmin, kmax
--
-- csound doc: <https://csound.com/docs/manual/outkpc.html>
outkpc ::  Sig -> Sig -> Sig -> Sig -> SE ()
outkpc b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "outkpc" [(Xr,[Kr,Kr,Kr,Kr])] [a1,a2,a3,a4]

-- Converters.

-- | 
-- Get the velocity of the current MIDI event.
--
-- > iamp  ampmidi  iscal [, ifn]
--
-- csound doc: <https://csound.com/docs/manual/ampmidi.html>
ampmidi ::  Msg -> D -> D
ampmidi _ b1 =
  D $ f <$> unD b1
  where
    f a1 = opcs "ampmidi" [(Ir,[Ir,Ir])] [a1]

-- | 

--
-- > igain  ampmidicurve  ivelocity, idynamicrange, iexponent
-- > kgain  ampmidicurve  kvelocity, kdynamicrange, kexponent
--
-- csound doc: <https://csound.com/docs/manual/ampmidicurve.html>
ampmidicurve ::  D -> D -> D -> Sig
ampmidicurve b1 b2 b3 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "ampmidicurve" [(Ir,[Ir,Ir,Ir]),(Kr,[Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Musically map MIDI velocity to peak amplitude within a specified dynamic range in decibels.
--
-- > iamplitude  ampmidid  ivelocity, idecibels
-- > kamplitude  ampmidid  kvelocity, idecibels
--
-- csound doc: <https://csound.com/docs/manual/ampmidid.html>
ampmidid ::  D -> D -> Sig
ampmidid b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "ampmidid" [(Ir,[Ir,Ir]),(Kr,[Kr,Ir])] [a1,a2]

-- | 
-- Get the note number of the current MIDI event, expressed in cycles-per-second.
--
-- > icps  cpsmidi 
--
-- csound doc: <https://csound.com/docs/manual/cpsmidi.html>
cpsmidi ::  Msg -> D
cpsmidi _  =
  D $ return $ f 
  where
    f  = opcs "cpsmidi" [(Ir,[])] []

-- | 
-- Get the note number of the current MIDI event and modify it by the current pitch-bend value, express it in cycles-per-second.
--
-- > icps  cpsmidib  [irange]
-- > kcps  cpsmidib  [irange]
--
-- csound doc: <https://csound.com/docs/manual/cpsmidib.html>
cpsmidib ::  Msg -> Sig
cpsmidib _  =
  Sig $ return $ f 
  where
    f  = opcs "cpsmidib" [(Ir,[Ir]),(Kr,[Ir])] []

-- | 
-- Get a MIDI note number (allows customized micro-tuning scales).
--
-- This unit is similar to cpsmidi, but allows fully customized micro-tuning scales.
--
-- > icps  cpstmid  ifn
--
-- csound doc: <https://csound.com/docs/manual/cpstmid.html>
cpstmid ::  Msg -> Tab -> D
cpstmid _ b1 =
  D $ f <$> unTab b1
  where
    f a1 = opcs "cpstmid" [(Ir,[Ir])] [a1]

-- | 
-- Get the note number, in octave-point-decimal units, of the current MIDI event.
--
-- > ioct  octmidi 
--
-- csound doc: <https://csound.com/docs/manual/octmidi.html>
octmidi ::  Msg -> D
octmidi _  =
  D $ return $ f 
  where
    f  = opcs "octmidi" [(Ir,[])] []

-- | 
-- Get the note number of the current MIDI event and modify it by the current pitch-bend value, express it in octave-point-decimal.
--
-- > ioct  octmidib  [irange]
-- > koct  octmidib  [irange]
--
-- csound doc: <https://csound.com/docs/manual/octmidib.html>
octmidib ::  Msg -> Sig
octmidib _  =
  Sig $ return $ f 
  where
    f  = opcs "octmidib" [(Ir,[Ir]),(Kr,[Ir])] []

-- | 
-- Get the note number of the current MIDI event, expressed in pitch-class units.
--
-- > ipch  pchmidi 
--
-- csound doc: <https://csound.com/docs/manual/pchmidi.html>
pchmidi ::  Msg -> D
pchmidi _  =
  D $ return $ f 
  where
    f  = opcs "pchmidi" [(Ir,[])] []

-- | 
-- Get the note number of the current MIDI event and modify it by the current pitch-bend value, express it in pitch-class units.
--
-- > ipch  pchmidib  [irange]
-- > kpch  pchmidib  [irange]
--
-- csound doc: <https://csound.com/docs/manual/pchmidib.html>
pchmidib ::  Msg -> Sig
pchmidib _  =
  Sig $ return $ f 
  where
    f  = opcs "pchmidib" [(Ir,[Ir]),(Kr,[Ir])] []

-- Generic I/O.

-- | 
-- Returns a generic MIDI message received by the MIDI IN port.
--
-- Returns a generic MIDI message received by the MIDI IN port
--
-- > kstatus, kchan, kdata1, kdata2  midiin 
--
-- csound doc: <https://csound.com/docs/manual/midiin.html>
midiin ::   (Sig,Sig,Sig,Sig)
midiin  =
  pureTuple $ return $ f 
  where
    f  = mopcs "midiin" ([Kr,Kr,Kr,Kr],[]) []

-- | 
-- Sends a generic MIDI message to the MIDI OUT port.
--
-- >  midiout  kstatus, kchan, kdata1, kdata2
--
-- csound doc: <https://csound.com/docs/manual/midiout.html>
midiout ::  Sig -> Sig -> Sig -> Sig -> SE ()
midiout b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "midiout" [(Xr,[Kr,Kr,Kr,Kr])] [a1,a2,a3,a4]

-- | 
-- Sends a generic MIDI message to the MIDI OUT port.
--
-- >  midiout_i  istatus, ichan, idata1, idata2
--
-- csound doc: <https://csound.com/docs/manual/midiout_i.html>
midiout_i ::  D -> D -> D -> D -> SE ()
midiout_i b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "midiout_i" [(Xr,[Ir,Ir,Ir,Ir])] [a1,a2,a3,a4]

-- Event Extenders.

-- | 

--
-- > kflag  lastcycle 
--
-- csound doc: <https://csound.com/docs/manual/lastcycle.html>
lastcycle ::   Sig
lastcycle  =
  Sig $ return $ f 
  where
    f  = opcs "lastcycle" [(Kr,[])] []

-- | 
-- Indicates whether a note is in its âreleaseâ stage.
--
-- Provides a way of knowing when a note off message for the current note is received. Only a noteoff message with the same MIDI note number as the one which triggered the note will be reported by release.
--
-- > kflag  release 
--
-- csound doc: <https://csound.com/docs/manual/release.html>
release ::   Sig
release  =
  Sig $ return $ f 
  where
    f  = opcs "release" [(Kr,[])] []

-- | 
-- Extend the duration of real-time generated events.
--
-- Extend the duration of real-time generated events and handle their extra life (Usually for usage along with release instead of linenr, linsegr, etc).
--
-- >  xtratim  iextradur
--
-- csound doc: <https://csound.com/docs/manual/xtratim.html>
xtratim ::  D -> SE ()
xtratim b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "xtratim" [(Xr,[Ir])] [a1]

-- Note Output.

-- | 

--
-- > kMidiNoteNum, kTrigger  midiarp  kRate[, kMode]
--
-- csound doc: <https://csound.com/docs/manual/midiarp.html>
midiarp ::  Sig -> (Sig,Sig)
midiarp b1 =
  pureTuple $ f <$> unSig b1
  where
    f a1 = mopcs "midiarp" ([Kr,Kr],[Kr,Kr]) [a1]

-- | 
-- Generates MIDI note messages at k-rate.
--
-- >  midion  kchn, knum, kvel
--
-- csound doc: <https://csound.com/docs/manual/midion.html>
midion ::  Sig -> Sig -> Sig -> SE ()
midion b1 b2 b3 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep_ "midion" [(Xr,[Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Sends noteon and noteoff messages to the MIDI OUT port.
--
-- Sends noteon and noteoff messages to the MIDI OUT port when triggered by a value different than zero.
--
-- >  midion2  kchn, knum, kvel, ktrig
--
-- csound doc: <https://csound.com/docs/manual/midion2.html>
midion2 ::  Sig -> Sig -> Sig -> Sig -> SE ()
midion2 b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "midion2" [(Xr,[Kr,Kr,Kr,Kr])] [a1,a2,a3,a4]

-- | 
-- Sends a stream of the MIDI notes.
--
-- >  moscil  kchn, knum, kvel, kdur, kpause
--
-- csound doc: <https://csound.com/docs/manual/moscil.html>
moscil ::  Sig -> Sig -> Sig -> Sig -> Sig -> SE ()
moscil b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "moscil" [(Xr,[Kr,Kr,Kr,Kr,Kr])] [a1,a2,a3,a4,a5]

-- | 
-- Send a noteoff message to the MIDI OUT port.
--
-- >  noteoff  ichn, inum, ivel
--
-- csound doc: <https://csound.com/docs/manual/noteoff.html>
noteoff ::  D -> D -> D -> SE ()
noteoff b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "noteoff" [(Xr,[Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Send a noteon message to the MIDI OUT port.
--
-- >  noteon  ichn, inum, ivel
--
-- csound doc: <https://csound.com/docs/manual/noteon.html>
noteon ::  D -> D -> D -> SE ()
noteon b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "noteon" [(Xr,[Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Sends a noteon and a noteoff MIDI message both with the same channel, number and velocity.
--
-- >  noteondur  ichn, inum, ivel, idur
--
-- csound doc: <https://csound.com/docs/manual/noteondur.html>
noteondur ::  D -> D -> D -> D -> SE ()
noteondur b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "noteondur" [(Xr,[Ir,Ir,Ir,Ir])] [a1,a2,a3,a4]

-- | 
-- Sends a noteon and a noteoff MIDI message both with the same channel, number and velocity.
--
-- >  noteondur2  ichn, inum, ivel, idur
--
-- csound doc: <https://csound.com/docs/manual/noteondur2.html>
noteondur2 ::  D -> D -> D -> D -> SE ()
noteondur2 b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "noteondur2" [(Xr,[Ir,Ir,Ir,Ir])] [a1,a2,a3,a4]

-- MIDI/Score Interoperability.

-- | 
-- Gets a MIDI channel's aftertouch value.
--
-- midichannelaftertouch is designed to simplify writing instruments that can be used interchangeably for either score or MIDI input, and to make it easier to adapt instruments originally written for score input to work with MIDI input.
--
-- >  midichannelaftertouch  xchannelaftertouch [, ilow] [, ihigh]
--
-- csound doc: <https://csound.com/docs/manual/midichannelaftertouch.html>
midichannelaftertouch ::  Sig -> SE ()
midichannelaftertouch b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "midichannelaftertouch" [(Xr,[Xr,Ir,Ir])] [a1]

-- | 
-- Returns the MIDI channel number from which the note was activated.
--
-- midichn returns the MIDI channel number (1 - 16) from which the note was activated. In the case of score notes, it returns 0.
--
-- > ichn  midichn 
--
-- csound doc: <https://csound.com/docs/manual/midichn.html>
midichn ::   D
midichn  =
  D $ return $ f 
  where
    f  = opcs "midichn" [(Ir,[])] []

-- | 
-- Gets a MIDI control change value.
--
-- midicontrolchange is designed to simplify writing instruments that can be used interchangeably for either score or MIDI input, and to make it easier to adapt instruments originally written for score input to work with MIDI input.
--
-- >  midicontrolchange  xcontroller, xcontrollervalue [, ilow] [, ihigh]
--
-- csound doc: <https://csound.com/docs/manual/midicontrolchange.html>
midicontrolchange ::  Sig -> Sig -> SE ()
midicontrolchange b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "midicontrolchange" [(Xr,[Xr,Xr,Ir,Ir])] [a1,a2]

-- | 
-- Changes values, depending on MIDI activation.
--
-- mididefault is designed to simplify writing instruments that can be used interchangeably for either score or MIDI input, and to make it easier to adapt instruments originally written for score input to work with MIDI input.
--
-- >  mididefault  xdefault, xvalue
--
-- csound doc: <https://csound.com/docs/manual/mididefault.html>
mididefault ::  Sig -> Sig -> SE ()
mididefault b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "mididefault" [(Xr,[Xr,Xr])] [a1,a2]

-- | 
-- Gets a MIDI noteoff value.
--
-- midinoteoff is designed to simplify writing instruments that can be used interchangeably for either score or MIDI input, and to make it easier to adapt instruments originally written for score input to work with MIDI input.
--
-- >  midinoteoff  xkey, xvelocity
--
-- csound doc: <https://csound.com/docs/manual/midinoteoff.html>
midinoteoff ::  Sig -> Sig -> SE ()
midinoteoff b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "midinoteoff" [(Xr,[Xr,Xr])] [a1,a2]

-- | 
-- Gets a MIDI note number as a cycles-per-second frequency.
--
-- midinoteoncps is designed to simplify writing instruments that can be used interchangeably for either score or MIDI input, and to make it easier to adapt instruments originally written for score input to work with MIDI input.
--
-- >  midinoteoncps  xcps, xvelocity
--
-- csound doc: <https://csound.com/docs/manual/midinoteoncps.html>
midinoteoncps ::  Sig -> Sig -> SE ()
midinoteoncps b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "midinoteoncps" [(Xr,[Xr,Xr])] [a1,a2]

-- | 
-- Gets a MIDI note number value.
--
-- midinoteonkey is designed to simplify writing instruments that can be used interchangeably for either score or MIDI input, and to make it easier to adapt instruments originally written for score input to work with MIDI input.
--
-- >  midinoteonkey  xkey, xvelocity
--
-- csound doc: <https://csound.com/docs/manual/midinoteonkey.html>
midinoteonkey ::  Sig -> Sig -> SE ()
midinoteonkey b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "midinoteonkey" [(Xr,[Xr,Xr])] [a1,a2]

-- | 
-- Gets a MIDI note number value as octave-point-decimal value.
--
-- midinoteonoct is designed to simplify writing instruments that can be used interchangeably for either score or MIDI input, and to make it easier to adapt instruments originally written for score input to work with MIDI input.
--
-- >  midinoteonoct  xoct, xvelocity
--
-- csound doc: <https://csound.com/docs/manual/midinoteonoct.html>
midinoteonoct ::  Sig -> Sig -> SE ()
midinoteonoct b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "midinoteonoct" [(Xr,[Xr,Xr])] [a1,a2]

-- | 
-- Gets a MIDI note number as a pitch-class value.
--
-- midinoteonpch is designed to simplify writing instruments that can be used interchangeably for either score or MIDI input, and to make it easier to adapt instruments originally written for score input to work with MIDI input.
--
-- >  midinoteonpch  xpch, xvelocity
--
-- csound doc: <https://csound.com/docs/manual/midinoteonpch.html>
midinoteonpch ::  Sig -> Sig -> SE ()
midinoteonpch b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "midinoteonpch" [(Xr,[Xr,Xr])] [a1,a2]

-- | 
-- Gets a MIDI pitchbend value.
--
-- midipitchbend is designed to simplify writing instruments that can be used interchangeably for either score or MIDI input, and to make it easier to adapt instruments originally written for score input to work with MIDI input.
--
-- >  midipitchbend  xpitchbend [, ilow] [, ihigh]
--
-- csound doc: <https://csound.com/docs/manual/midipitchbend.html>
midipitchbend ::  Sig -> SE ()
midipitchbend b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "midipitchbend" [(Xr,[Xr,Ir,Ir])] [a1]

-- | 
-- Gets a MIDI polyphonic aftertouch value.
--
-- midipolyaftertouch is designed to simplify writing instruments that can be used interchangeably for either score or MIDI input, and to make it easier to adapt instruments originally written for score input to work with MIDI input.
--
-- >  midipolyaftertouch  xpolyaftertouch, xkey [, ilow] [, ihigh]
--
-- csound doc: <https://csound.com/docs/manual/midipolyaftertouch.html>
midipolyaftertouch ::  Sig -> Sig -> SE ()
midipolyaftertouch b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "midipolyaftertouch" [(Xr,[Xr,Xr,Ir,Ir])] [a1,a2]

-- | 
-- Gets a MIDI program change value.
--
-- midiprogramchange is designed to simplify writing instruments that can be used interchangeably for either score or MIDI input, and to make it easier to adapt instruments originally written for score input to work with MIDI input.
--
-- >  midiprogramchange  xprogram
--
-- csound doc: <https://csound.com/docs/manual/midiprogramchange.html>
midiprogramchange ::  Sig -> SE ()
midiprogramchange b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "midiprogramchange" [(Xr,[Xr])] [a1]

-- System Realtime.

-- | 
-- Sends a MIDI CLOCK message.
--
-- >  mclock  ifreq
--
-- csound doc: <https://csound.com/docs/manual/mclock.html>
mclock ::  D -> SE ()
mclock b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "mclock" [(Xr,[Ir])] [a1]

-- | 
-- Send system real-time messages to the MIDI OUT port.
--
-- >  mrtmsg  imsgtype
--
-- csound doc: <https://csound.com/docs/manual/mrtmsg.html>
mrtmsg ::  D -> SE ()
mrtmsg b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "mrtmsg" [(Xr,[Ir])] [a1]