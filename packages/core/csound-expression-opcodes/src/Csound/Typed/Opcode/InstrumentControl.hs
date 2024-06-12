module Csound.Typed.Opcode.InstrumentControl (
    
    
    -- * Clock Control.
    clockoff, clockon,
    
    -- * Compilation.
    compilecsd, compileorc, compilestr, evalstr,
    
    -- * Duration Control.
    ihold, turnoff, turnoff2_i, turnoff3, turnon,
    
    -- * Invocation.
    event, event_i, mute, nstance, readscore, remove, schedkwhen, schedkwhennamed, schedule, schedulek, schedwhen, scoreline, scoreline_i,
    
    -- * Realtime Performance Control.
    active, cpumeter, cpuprc, exitnow, jacktransport, maxalloc, prealloc,
    
    -- * Sensing and Control.
    button, changed, changed2, checkbox, cntDelete, cntDelete_i, cntCreate, cntCycles, cntRead, cntReset, cntState, control, count, count_i, follow, follow2, getcfg, joystick, metro, metro2, metrobpm, midifilestatus, miditempo, p5gconnect, p5gdata, pcount, peak, pindex, pitch, pitchamdf, plltrack, ptrack, readscratch, rewindscore, rms, sensekey, seqtime, seqtime2, sequ, setctrl, setscorepos, splitrig, tempest, tempo, tempoval, timedseq, trigger, trigseq, vactrol, wiiconnect, wiidata, wiirange, wiisend, writescratch, xyin,
    
    -- * Stacks.
    pop, pop_f, push, push_f, stack,
    
    -- * Subinstrument Control.
    subinstr, subinstrinit,
    
    -- * Time Reading.
    date, dates, elapsedcycles, elapsedtime, eventcycles, eventtime, readclock, rtclock, timeinstk, timeinsts, timek, times) where

import Control.Monad.Trans.Class
import Control.Monad
import Csound.Dynamic
import Csound.Typed

-- Clock Control.

-- | 
-- Stops one of a number of internal clocks.
--
-- >  clockoff  inum
--
-- csound doc: <https://csound.com/docs/manual/clockoff.html>
clockoff ::  D -> SE ()
clockoff b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "clockoff" [(Xr,[Ir])] [a1]

-- | 
-- Starts one of a number of internal clocks.
--
-- >  clockon  inum
--
-- csound doc: <https://csound.com/docs/manual/clockon.html>
clockon ::  D -> SE ()
clockon b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "clockon" [(Xr,[Ir])] [a1]

-- Compilation.

-- | 
-- compiles a new orchestra from an ASCII file
--
-- Compilecsd will read a CSD file and compile one or more instruments at init time, which will be added to the running engine. In case of existing instrument numbers or names, these will be replaced, but any instance still running of the old instrument definition will still perform until it terminates. In addition, it will read the score (if it exists) contained in the CSD file and add it to the list of events to be performed by Csound. The opcode ignores any section in the CSD file that is not the orchestra or the score.
--
-- > ires  compilecsd  Sfilename
--
-- csound doc: <https://csound.com/docs/manual/compilecsd.html>
compilecsd ::  Str -> D
compilecsd b1 =
  D $ f <$> unStr b1
  where
    f a1 = opcs "compilecsd" [(Ir,[Sr])] [a1]

-- | 
-- compiles a new orchestra from an ASCII file
--
-- Compileorc will compile one or more instruments at init time, which will be added to the running engine. In case of
-- existing instrument numbers or names, these will be replaced, but any instance still running of the old instrument
-- definition will still perform until it terminates.
--
-- > ires  compileorc  Sfilename
--
-- csound doc: <https://csound.com/docs/manual/compileorc.html>
compileorc ::  Str -> D
compileorc b1 =
  D $ f <$> unStr b1
  where
    f a1 = opcs "compileorc" [(Ir,[Sr])] [a1]

-- | 
-- compiles a new orchestra passed in as an ASCII string
--
-- Compilestr will compile one or more instruments at init time, which will be added to the running engine. In case of
-- existing instrument numbers or names, these will be replaced, but any instance still running of the old instrument
-- definition will still perform until it terminates. Only new instances will use the new definition.
-- Multi-line strings are accepted, using {{  }} to enclose the string.
--
-- > ires  compilestr  Sorch
--
-- csound doc: <https://csound.com/docs/manual/compilestr.html>
compilestr ::  Str -> D
compilestr b1 =
  D $ f <$> unStr b1
  where
    f a1 = opcs "compilestr" [(Ir,[Sr])] [a1]

-- | 
-- Evalstrs evaluates a string containing Csound code, returning a value.
--
-- Evalstr compiles and runs Csound code and returns a value from the global space (instr 0). This opcode can be
-- also used to compile new instruments (as compilestr).
--
-- > ires  evalstr  Scode 
-- > kres  evalstr  Scode, ktrig 
--
-- csound doc: <https://csound.com/docs/manual/evalstr.html>
evalstr ::  Str -> Sig
evalstr b1 =
  Sig $ f <$> unStr b1
  where
    f a1 = opcs "evalstr" [(Ir,[Sr]),(Kr,[Sr,Kr])] [a1]

-- Duration Control.

-- | 
-- Creates a held note.
--
-- Causes a finite-duration note to become a âheldâ note
--
-- >  ihold 
--
-- csound doc: <https://csound.com/docs/manual/ihold.html>
ihold ::   SE ()
ihold  =
  SE $ join $ return $ f 
  where
    f  = opcsDep_ "ihold" [(Xr,[])] []

-- | 
-- Enables an instrument to turn itself off or to turn an instance of another instrument off.
--
-- >  turnoff 
-- >  turnoff  inst
-- >  turnoff  knst
--
-- csound doc: <https://csound.com/docs/manual/turnoff.html>
turnoff ::   SE ()
turnoff  =
  SE $ join $ return $ f 
  where
    f  = opcsDep_ "turnoff" [(Xr,[])] []

-- | 

--
-- >  turnoff2_i  insno, imode, irelease
-- >         
--
-- csound doc: <https://csound.com/docs/manual/turnoff2.html>
turnoff2_i ::  D -> D -> D -> SE ()
turnoff2_i b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "turnoff2_i" [(Xr,[Ir,Ir,Ir])] [a1,a2,a3]

-- | 

--
-- >  turnoff3  kinsno
-- >         
--
-- csound doc: <https://csound.com/docs/manual/turnoff3.html>
turnoff3 ::  Sig -> SE ()
turnoff3 b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "turnoff3" [(Xr,[Kr])] [a1]

-- | 
-- Activate an instrument for an indefinite time.
--
-- >  turnon  insnum [, itime]
--
-- csound doc: <https://csound.com/docs/manual/turnon.html>
turnon ::  D -> SE ()
turnon b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "turnon" [(Xr,[Ir,Ir])] [a1]

-- Invocation.

-- | 
-- Generates a score event from an instrument.
--
-- >  event  "scorechar", kinsnum, kdelay, kdur, [, kp4] [, kp5] [, ...]
-- >  event  "scorechar", "insname", kdelay, kdur, [, kp4] [, kp5] [, ...]
--
-- csound doc: <https://csound.com/docs/manual/event.html>
event ::  Str -> Sig -> Sig -> Sig -> [Sig] -> SE ()
event b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> mapM (lift . unSig) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "event" [(Xr,[Sr] ++ (repeat Kr))] ([a1,a2,a3,a4] ++ a5)

-- | 
-- Generates a score event from an instrument.
--
-- >  event_i  "scorechar", iinsnum, idelay, idur, [, ip4] [, ip5] [, ...]
-- >  event_i  "scorechar", "insname", idelay, idur, [, ip4] [, ip5] [, ...]
--
-- csound doc: <https://csound.com/docs/manual/event_i.html>
event_i ::  Str -> D -> D -> D -> [D] -> SE ()
event_i b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> mapM (lift . unD) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "event_i" [(Xr,[Sr] ++ (repeat Ir))] ([a1,a2,a3,a4] ++ a5)

-- | 
-- Mutes/unmutes new instances of a given instrument.
--
-- >  mute  insnum [, iswitch]
-- >  mute  "insname" [, iswitch]
--
-- csound doc: <https://csound.com/docs/manual/mute.html>
mute ::  D -> SE ()
mute b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "mute" [(Xr,[Ir,Ir])] [a1]

-- | 
-- Schedules a new instrument instance, storing the instance handle in a variable.
--
-- Schedules a new instrument nstance, returning a handle that can be used later to
-- refer directly to the running nstance. This opcode is similar to schedule, but has the added
-- facility of retrieving the nstance handle.
--
-- > iHandle  nstance  insnum, iwhen, idur [, ip4] [, ip5] [...]
-- > iHandle  nstance  "insname", iwhen, idur [, ip4] [, ip5] [...]
--
-- csound doc: <https://csound.com/docs/manual/nstance.html>
nstance ::  D -> D -> D -> D
nstance b1 b2 b3 =
  D $ f <$> unD b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "nstance" [(Ir,(repeat Ir)),(Ir,[Sr] ++ (repeat Ir))] [a1,a2,a3]

-- | 
-- Read, preprocess and schedule a score from an input string.
--
-- Readscore will issue one or more score events. 
-- 	  It can handle strings in the same conditions as
-- 	  the standard score, including preprocessing (carry, sort, ramp, etc). 
-- Multi-line strings are accepted, using {{  }} to enclose the string.
--
-- >  readscore  Sin 
--
-- csound doc: <https://csound.com/docs/manual/readscore.html>
readscore ::  Str -> SE ()
readscore b1 =
  SE $ join $ f <$> (lift . unStr) b1
  where
    f a1 = opcsDep_ "readscore" [(Xr,[Sr])] [a1]

-- | 
-- Removes the definition of an instrument.
--
-- Removes the definition of an instrument as long as it is not in use.
--
-- >  remove  insnum
--
-- csound doc: <https://csound.com/docs/manual/remove.html>
remove ::  D -> SE ()
remove b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "remove" [(Xr,[Ir])] [a1]

-- | 
-- Adds a new score event generated by a k-rate trigger.
--
-- >  schedkwhen  ktrigger, kmintim, kmaxnum, kinsnum, kwhen, kdur \
-- >           [, ip4] [, ip5] [...]
-- >  schedkwhen  ktrigger, kmintim, kmaxnum, "insname", kwhen, kdur \
-- >           [, ip4] [, ip5] [...]
--
-- csound doc: <https://csound.com/docs/manual/schedkwhen.html>
schedkwhen ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> SE ()
schedkwhen b1 b2 b3 b4 b5 b6 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5 <*> (lift . unSig) b6
  where
    f a1 a2 a3 a4 a5 a6 = opcsDep_ "schedkwhen" [(Xr,[Kr,Kr,Kr,Kr,Kr,Kr] ++ (repeat Ir))] [a1
                                                                                          ,a2
                                                                                          ,a3
                                                                                          ,a4
                                                                                          ,a5
                                                                                          ,a6]

-- | 
-- Similar to schedkwhen but uses a named instrument at init-time.
--
-- >  schedkwhennamed  ktrigger, kmintim, kmaxnum, "name", kwhen, kdur \
-- >           [, ip4] [, ip5] [...]
--
-- csound doc: <https://csound.com/docs/manual/schedkwhennamed.html>
schedkwhennamed ::  Sig -> Sig -> Sig -> Str -> Sig -> Sig -> SE ()
schedkwhennamed b1 b2 b3 b4 b5 b6 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unStr) b4 <*> (lift . unSig) b5 <*> (lift . unSig) b6
  where
    f a1 a2 a3 a4 a5 a6 = opcsDep_ "schedkwhennamed" [(Xr,[Kr,Kr,Kr,Sr,Kr,Kr] ++ (repeat Ir))] [a1
                                                                                               ,a2
                                                                                               ,a3
                                                                                               ,a4
                                                                                               ,a5
                                                                                               ,a6]

-- | 
-- Adds a new score event.
--
-- >  schedule  insnum, iwhen, idur [, ip4] [, ip5] [...]
-- >  schedule  "insname", iwhen,
-- >         idur [, ip4] [, ip5] [...]
-- >  schedule  iPar[]
--
-- csound doc: <https://csound.com/docs/manual/schedule.html>
schedule ::  D -> D -> D -> SE ()
schedule b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "schedule" [(Xr,(repeat Ir))] [a1,a2,a3]

-- | 

--
-- >  schedulek  knsnum, kwhen, kdur [, kp4] [, kp5] [...]
-- >  schedulek  "insname", kwhen,
-- >         kdur [, kp4] [, kp5] [...]
--
-- csound doc: <https://csound.com/docs/manual/schedulek.html>
schedulek ::  Sig -> Sig -> Sig -> SE ()
schedulek b1 b2 b3 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep_ "schedulek" [(Xr,(repeat Kr))] [a1,a2,a3]

-- | 
-- Adds a new score event.
--
-- >  schedwhen  ktrigger, kinsnum, kwhen, kdur [, ip4] [, ip5] [...]
-- >  schedwhen  ktrigger, "insname", kwhen, kdur [, ip4] [, ip5] [...]
--
-- csound doc: <https://csound.com/docs/manual/schedwhen.html>
schedwhen ::  Sig -> Sig -> Sig -> Sig -> SE ()
schedwhen b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "schedwhen" [(Xr,[Kr,Kr,Kr,Kr] ++ (repeat Ir))] [a1,a2,a3,a4]

-- | 
-- Issues one or more score line events from an instrument.
--
-- Scoreline will issue one or more score events, if ktrig is 1 every k-period. 
-- 	  It can handle strings in the same conditions as
-- 	  the standard score. Multi-line strings are accepted, using {{  }} to enclose the string.
--
-- >  scoreline  Sin, ktrig
--
-- csound doc: <https://csound.com/docs/manual/scoreline.html>
scoreline ::  Str -> Sig -> SE ()
scoreline b1 b2 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "scoreline" [(Xr,[Sr,Kr])] [a1,a2]

-- | 
-- Issues one or more score line events from an instrument at i-time.
--
-- scoreline_i will issue score events at i-time.  It can handle strings in the same conditions as the standard score. Multi-line strings are accepted, using {{  }} to enclose the string.
--
-- >  scoreline_i  Sin
--
-- csound doc: <https://csound.com/docs/manual/scoreline_i.html>
scoreline_i ::  Str -> SE ()
scoreline_i b1 =
  SE $ join $ f <$> (lift . unStr) b1
  where
    f a1 = opcsDep_ "scoreline_i" [(Xr,[Sr])] [a1]

-- Realtime Performance Control.

-- | 
-- Returns the number of active instances of an instrument.
--
-- Returns the number of active instances of an instrument with
--       options to ignore releasing instances.
--
-- > ir  active  insnum [,iopt [,inorel]]
-- > ir  active  Sinsname [,iopt [,inorel]]
-- > kres  active  kinsnum [,iopt [,inorel]]
--
-- csound doc: <https://csound.com/docs/manual/active.html>
active ::  D -> Sig
active b1 =
  Sig $ f <$> unD b1
  where
    f a1 = opcs "active" [(Ir,[Ir,Ir,Ir]),(Ir,[Sr,Ir,Ir]),(Kr,[Kr,Ir,Ir])] [a1]

-- | 
-- Reports the usage of cpu either total or per core.
--
-- Reports the usage of cpu either total or per core to monitor how
--       close to max-out the processing is.
--
-- > ktot[,kcpu1, kcpu2,...] cpumeter  ifreq
--
-- csound doc: <https://csound.com/docs/manual/cpumeter.html>
cpumeter :: forall a . Tuple a => D -> a
cpumeter b1 =
  pureTuple $ f <$> unD b1
  where
    f a1 = mopcs "cpumeter" ((repeat Kr),[Ir]) [a1]

-- | 
-- Control allocation of cpu resources on a per-instrument basis, to optimize realtime output.
--
-- >  cpuprc  insnum, ipercent
-- >  cpuprc  Sinsname, ipercent
--
-- csound doc: <https://csound.com/docs/manual/cpuprc.html>
cpuprc ::  D -> D -> SE ()
cpuprc b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "cpuprc" [(Xr,[Ir,Ir])] [a1,a2]

-- | 
-- Exit Csound as fast as possible, with no cleaning up.
--
-- In Csound4 calls an exit function to leave Csound as fast as
--     possible. On Csound5 exits back to the driving code.
--
-- >  exitnow  [ivalue]
--
-- csound doc: <https://csound.com/docs/manual/exitnow.html>
exitnow ::   SE ()
exitnow  =
  SE $ join $ return $ f 
  where
    f  = opcsDep_ "exitnow" [(Xr,[Ir])] []

-- | 
-- Start/stop jack_transport and can optionally relocate the playback head.
--
-- >  jacktransport  icommand [, ilocation]
--
-- csound doc: <https://csound.com/docs/manual/jacktransport.html>
jacktransport ::  D -> SE ()
jacktransport b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "jacktransport" [(Xr,[Ir,Ir])] [a1]

-- | 
-- Limits the number of allocations of an instrument.
--
-- >  maxalloc  insnum, icount
-- >  maxalloc  Sinsname, icount
--
-- csound doc: <https://csound.com/docs/manual/maxalloc.html>
maxalloc ::  D -> D -> SE ()
maxalloc b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "maxalloc" [(Xr,[Ir,Ir])] [a1,a2]

-- | 
-- Creates space for instruments but does not run them.
--
-- >  prealloc  insnum, icount
-- >  prealloc  "insname", icount
--
-- csound doc: <https://csound.com/docs/manual/prealloc.html>
prealloc ::  D -> D -> SE ()
prealloc b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "prealloc" [(Xr,[Ir,Ir])] [a1,a2]

-- Sensing and Control.

-- | 
-- Sense on-screen controls.
--
-- Sense on-screen controls. Requires Winsound or TCL/TK.
--
-- > kres  button  knum
--
-- csound doc: <https://csound.com/docs/manual/button.html>
button ::  Sig -> Sig
button b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "button" [(Kr,[Kr])] [a1]

-- | 
-- k-rate signal change detector.
--
-- This opcode outputs a trigger signal that informs when any one of its k-rate arguments has changed. Useful with valuator widgets or MIDI controllers.
--
-- > ktrig  changed  kvar1 [, kvar2,..., kvarN]
--
-- csound doc: <https://csound.com/docs/manual/changed.html>
changed ::  [Sig] -> Sig
changed b1 =
  Sig $ f <$> mapM unSig b1
  where
    f a1 = opcs "changed" [(Kr,(repeat Kr))] a1

-- | 
-- k-rate signal change detector.
--
-- This opcode outputs a trigger signal that informs when any one
--         of its k-rate arguments has changed, or a value in an array. Useful with valuator widgets or MIDI controllers.
--
-- > ktrig  changed2  kvar1 [, kvar2,..., kvarN]
-- > ktrig  changed2  karr[]
-- > ktrig  changed2  aarr[]
--
-- csound doc: <https://csound.com/docs/manual/changed2.html>
changed2 ::  Sig -> Sig
changed2 b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "changed2" [(Kr,(repeat Kr)),(Kr,[Kr]),(Kr,[Ar])] [a1]

-- | 
-- Sense on-screen controls.
--
-- Sense on-screen controls. Requires Winsound or TCL/TK.
--
-- > kres  checkbox  knum
--
-- csound doc: <https://csound.com/docs/manual/checkbox.html>
checkbox ::  Sig -> Sig
checkbox b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "checkbox" [(Kr,[Kr])] [a1]

-- | 

--
-- > kval  cntDelete  icnt
--
-- csound doc: <https://csound.com/docs/manual/cntDelete.html>
cntDelete ::  D -> Sig
cntDelete b1 =
  Sig $ f <$> unD b1
  where
    f a1 = opcs "cntDelete" [(Kr,[Ir])] [a1]

-- | 

--
-- > ival  cntDelete_i  icnt
--
-- csound doc: <https://csound.com/docs/manual/cntDelete_i.html>
cntDelete_i ::  D -> D
cntDelete_i b1 =
  D $ f <$> unD b1
  where
    f a1 = opcs "cntDelete_i" [(Ir,[Ir])] [a1]

-- | 

--
-- > icnt  cntCreate  [imax, imin, inc]
--
-- csound doc: <https://csound.com/docs/manual/cntCreate.html>
cntCreate ::   D
cntCreate  =
  D $ return $ f 
  where
    f  = opcs "cntCreate" [(Ir,[Ir,Ir,Ir])] []

-- | 

--
-- > kval  cntCycles  icnt
--
-- csound doc: <https://csound.com/docs/manual/cntCycles.html>
cntCycles ::  D -> Sig
cntCycles b1 =
  Sig $ f <$> unD b1
  where
    f a1 = opcs "cntCycles" [(Kr,[Ir])] [a1]

-- | 

--
-- > kval  cntRead icnt
--
-- csound doc: <https://csound.com/docs/manual/cntRead.html>
cntRead ::  D -> Sig
cntRead b1 =
  Sig $ f <$> unD b1
  where
    f a1 = opcs "cntRead" [(Kr,[Ir])] [a1]

-- | 

--
-- >  cntReset icnt
--
-- csound doc: <https://csound.com/docs/manual/cntReset.html>
cntReset ::  D -> SE ()
cntReset b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "cntReset" [(Xr,[Ir])] [a1]

-- | 

--
-- > kmax, kmin, kinc  cntState icnt
--
-- csound doc: <https://csound.com/docs/manual/cntState.html>
cntState ::  D -> (Sig,Sig,Sig)
cntState b1 =
  pureTuple $ f <$> unD b1
  where
    f a1 = mopcs "cntState" ([Kr,Kr,Kr],[Ir]) [a1]

-- | 
-- Configurable slider controls for realtime user input.
--
-- Configurable slider controls for realtime user input. Requires Winsound or TCL/TK. control reads a slider's value.
--
-- > kres  control  knum
--
-- csound doc: <https://csound.com/docs/manual/control.html>
control ::  Sig -> Sig
control b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "control" [(Kr,[Kr])] [a1]

-- | 

--
-- > kval  count  icnt
--
-- csound doc: <https://csound.com/docs/manual/count.html>
count ::  D -> Sig
count b1 =
  Sig $ f <$> unD b1
  where
    f a1 = opcs "count" [(Kr,[Ir])] [a1]

-- | 

--
-- > ival  count_i  icnt
--
-- csound doc: <https://csound.com/docs/manual/count_i.html>
count_i ::  D -> D
count_i b1 =
  D $ f <$> unD b1
  where
    f a1 = opcs "count_i" [(Ir,[Ir])] [a1]

-- | 
-- Envelope follower unit generator.
--
-- > ares  follow  asig, idt
--
-- csound doc: <https://csound.com/docs/manual/follow.html>
follow ::  Sig -> D -> Sig
follow b1 b2 =
  Sig $ f <$> unSig b1 <*> unD b2
  where
    f a1 a2 = opcs "follow" [(Ar,[Ar,Ir])] [a1,a2]

-- | 
-- Another controllable envelope extractor.
--
-- A controllable envelope extractor using the algorithm attributed to Jean-Marc Jot.
--
-- > ares  follow2  asig, katt, krel
--
-- csound doc: <https://csound.com/docs/manual/follow2.html>
follow2 ::  Sig -> Sig -> Sig -> Sig
follow2 b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "follow2" [(Ar,[Ar,Kr,Kr])] [a1,a2,a3]

-- | 
-- Return Csound settings.
--
-- Return various configuration settings in Svalue as a string at init time.
--
-- > Svalue  getcfg  iopt
--
-- csound doc: <https://csound.com/docs/manual/getcfg.html>
getcfg ::  D -> Str
getcfg b1 =
  Str $ f <$> unD b1
  where
    f a1 = opcs "getcfg" [(Sr,[Ir])] [a1]

-- | 
-- Reads data from a joystick controller.
--
-- Reads data from a Linux joystick controller
--
-- > kres  joystick  kdevice ktab
--
-- csound doc: <https://csound.com/docs/manual/joystick.html>
joystick ::  Sig -> Tab -> Sig
joystick b1 b2 =
  Sig $ f <$> unSig b1 <*> unTab b2
  where
    f a1 a2 = opcs "joystick" [(Kr,[Kr,Kr])] [a1,a2]

-- | 
-- Trigger Metronome
--
-- Generate a metronomic signal to be used in any circumstance an isochronous trigger is needed.
--
-- > ktrig   metro   kfreq [, initphase]
--
-- csound doc: <https://csound.com/docs/manual/metro.html>
metro ::  Sig -> Sig
metro b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "metro" [(Kr,[Kr,Ir])] [a1]

-- | 

--
-- > ktrig   metro2   kfreq, kswing [, iamp, initphase]
--
-- csound doc: <https://csound.com/docs/manual/metro2.html>
metro2 ::  Sig -> Sig -> Sig
metro2 b1 b2 =
  Sig $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = opcs "metro2" [(Kr,[Kr,Kr,Ir,Ir])] [a1,a2]

-- | 

--
-- > ktrig   metrobpm   kfreq [, initphase] [, kgate]
--
-- csound doc: <https://csound.com/docs/manual/metrobpm.html>
metrobpm ::  Sig -> Sig
metrobpm b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "metrobpm" [(Kr,[Kr,Ir,Kr])] [a1]

-- | 
-- Returns the playback status of MIDI file input.
--
-- Returns the current playback status at k-rate, of the input MIDI file, 1 if file is playing, 0 if the end-of-the file
-- has been reached.
--
-- > ksig   midifilestatus 
--
-- csound doc: <https://csound.com/docs/manual/midifilestatus.html>
midifilestatus ::   Sig
midifilestatus  =
  Sig $ return $ f 
  where
    f  = opcs "midifilestatus" [(Kr,[])] []

-- | 
-- Returns the current tempo at k-rate, of either the MIDI file (if available) or the score
--
-- > ksig   miditempo 
--
-- csound doc: <https://csound.com/docs/manual/miditempo.html>
miditempo ::   Sig
miditempo  =
  Sig $ return $ f 
  where
    f  = opcs "miditempo" [(Kr,[])] []

-- | 
-- Reads data from a P5 Glove controller.
--
-- Opens and at control-rate polls a P5 Glove controller.
--
-- >  p5gconnect 
--
-- csound doc: <https://csound.com/docs/manual/p5gconnect.html>
p5gconnect ::   SE ()
p5gconnect  =
  SE $ join $ return $ f 
  where
    f  = opcsDep_ "p5gconnect" [(Xr,[])] []

-- | 
-- Reads data fields from an external P5 Glove.
--
-- Reads data fields from a P5 Glove controller.
--
-- > kres  p5gdata  kcontrol
--
-- csound doc: <https://csound.com/docs/manual/p5gdata.html>
p5gdata ::  Sig -> Sig
p5gdata b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "p5gdata" [(Kr,[Kr])] [a1]

-- | 
-- Returns the number of pfields belonging to a note event.
--
-- pcount returns the number of pfields belonging to a note event.
--
-- > icount  pcount 
--
-- csound doc: <https://csound.com/docs/manual/pcount.html>
pcount ::   D
pcount  =
  D $ return $ f 
  where
    f  = opcs "pcount" [(Ir,[])] []

-- | 
-- Maintains the output equal to the highest absolute value received.
--
-- These opcodes maintain the output k-rate variable as the peak absolute level so far received.
--
-- > kres  peak  asig
-- > kres  peak  ksig
--
-- csound doc: <https://csound.com/docs/manual/peak.html>
peak ::  Sig -> Sig
peak b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "peak" [(Kr,[Ar]),(Kr,[Kr])] [a1]

-- | 
-- Returns the value of a specified pfield.
--
-- pindex returns the value of a specified pfield.
--
-- > ivalue  pindex  ipfieldIndex
--
-- csound doc: <https://csound.com/docs/manual/pindex.html>
pindex ::  D -> D
pindex b1 =
  D $ f <$> unD b1
  where
    f a1 = opcs "pindex" [(Ir,[Ir])] [a1]

-- | 
-- Tracks the pitch of a signal.
--
-- Using the same techniques as spectrum and specptrk, pitch tracks the pitch of the signal in octave point decimal form, and amplitude in dB.
--
-- > koct, kamp  pitch  asig, iupdte, ilo, ihi, idbthresh [, ifrqs] [, iconf] \
-- >           [, istrt] [, iocts] [, iq] [, inptls] [, irolloff] [, iskip]
--
-- csound doc: <https://csound.com/docs/manual/pitch.html>
pitch ::  Sig -> D -> D -> D -> D -> (Sig,Sig)
pitch b1 b2 b3 b4 b5 =
  pureTuple $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5
  where
    f a1 a2 a3 a4 a5 = mopcs "pitch" ([Kr,Kr],[Ar,Ir,Ir,Ir,Ir,Ir,Ir,Ir,Ir,Ir,Ir,Ir,Ir]) [a1
                                                                                        ,a2
                                                                                        ,a3
                                                                                        ,a4
                                                                                        ,a5]

-- | 
-- Follows the pitch of a signal based on the AMDF method.
--
-- Follows the pitch of a signal based on the AMDF method (Average Magnitude Difference Function). Outputs pitch and amplitude tracking signals. The method is quite fast and should run in realtime. This technique usually works best for monophonic signals.
--
-- > kcps, krms  pitchamdf  asig, imincps, imaxcps [, icps] [, imedi] \
-- >           [, idowns] [, iexcps] [, irmsmedi]
--
-- csound doc: <https://csound.com/docs/manual/pitchamdf.html>
pitchamdf ::  Sig -> D -> D -> (Sig,Sig)
pitchamdf b1 b2 b3 =
  pureTuple $ f <$> unSig b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = mopcs "pitchamdf" ([Kr,Kr],[Ar,Ir,Ir,Ir,Ir,Ir,Ir,Ir]) [a1,a2,a3]

-- | 
-- Tracks the pitch of a signal.
--
-- plltrack, a pitch tracker based on a phase-locked loop algorithm, described in Zolzer, U, Sankarababu, S.V. and Moller, S, "PLL-based Pitch Detection and Tracking for Audio Signals. Proc. of IIH-MSP 2012".
--
-- > acps, alock  plltrack  asig, kd [, kloopf, kloopq, klf, khf, kthresh]
--
-- csound doc: <https://csound.com/docs/manual/plltrack.html>
plltrack ::  Sig -> Sig -> (Sig,Sig)
plltrack b1 b2 =
  pureTuple $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = mopcs "plltrack" ([Ar,Ar],[Ar,Kr,Kr,Kr,Kr,Kr,Kr]) [a1,a2]

-- | 
-- Tracks the pitch of a signal.
--
-- ptrack takes an input signal, splits it into ihopsize blocks and using a STFT method, extracts an estimated pitch for its fundamental frequency as well as estimating the total amplitude of the signal in dB, relative to full-scale (0dB). The method implies an analysis window size of 2*ihopsize samples (overlaping by 1/2 window), which has to be a power-of-two, between 128 and 8192 (hopsizes between 64 and 4096). Smaller windows will give better time precision, but worse frequency accuracy (esp. in low fundamentals).This opcode is based on an original algorithm by M. Puckette.
--
-- > kcps, kamp  ptrack  asig, ihopsize[,ipeaks]
--
-- csound doc: <https://csound.com/docs/manual/ptrack.html>
ptrack ::  Sig -> D -> (Sig,Sig)
ptrack b1 b2 =
  pureTuple $ f <$> unSig b1 <*> unD b2
  where
    f a1 a2 = mopcs "ptrack" ([Kr,Kr],[Ar,Ir,Ir]) [a1,a2]

-- | 
-- returns a value stored in the instance of an instrument.
--
-- The readscratch opcode returns one of four
--       scalar values stored in the instance of an instrument.
--
-- > ival  readscratch [index]
--
-- csound doc: <https://csound.com/docs/manual/readscratch.html>
readscratch ::   D
readscratch  =
  D $ return $ f 
  where
    f  = opcs "readscratch" [(Ir,[Ir])] []

-- | 
-- Rewinds the playback position of the current score performance.
--
-- Rewinds the playback position of the current score performance..
--
-- >   rewindscore 
--
-- csound doc: <https://csound.com/docs/manual/rewindscore.html>
rewindscore ::   SE ()
rewindscore  =
  SE $ join $ return $ f 
  where
    f  = opcsDep_ "rewindscore" [(Xr,[])] []

-- | 
-- Determines the root-mean-square amplitude of an audio signal.
--
-- Determines the root-mean-square amplitude of an audio signal. It low-pass filters the actual value, to average in the manner of a VU meter.
--
-- > kres  rms  asig [, ihp] [, iskip]
--
-- csound doc: <https://csound.com/docs/manual/rms.html>
rms ::  Sig -> Sig
rms b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "rms" [(Kr,[Ar,Ir,Ir])] [a1]

-- | 
-- Returns the ASCII code of a key that has been pressed.
--
-- Returns the ASCII code of a key that has been pressed, or -1 if no key has been pressed.
--
-- > kres[, kkeydown]  sensekey 
--
-- csound doc: <https://csound.com/docs/manual/sensekey.html>
sensekey :: forall a . Tuple a =>  a
sensekey  =
  pureTuple $ return $ f 
  where
    f  = mopcs "sensekey" ([Kr,Kr],[]) []

-- | 
-- Generates a trigger signal according to the values stored in a table.
--
-- > ktrig_out  seqtime  ktime_unit, kstart, kloop, kinitndx, kfn_times
--
-- csound doc: <https://csound.com/docs/manual/seqtime.html>
seqtime ::  Sig -> Sig -> Sig -> Sig -> Tab -> Sig
seqtime b1 b2 b3 b4 b5 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unTab b5
  where
    f a1 a2 a3 a4 a5 = opcs "seqtime" [(Kr,[Kr,Kr,Kr,Kr,Kr])] [a1,a2,a3,a4,a5]

-- | 
-- Generates a trigger signal according to the values stored in a table.
--
-- > ktrig_out  seqtime2  ktrig_in, ktime_unit, kstart, kloop, kinitndx, kfn_times
--
-- csound doc: <https://csound.com/docs/manual/seqtime2.html>
seqtime2 ::  Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Sig
seqtime2 b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unTab b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "seqtime2" [(Kr,[Kr,Kr,Kr,Kr,Kr,Kr])] [a1,a2,a3,a4,a5,a6]

-- | 

--
-- > kres   sequ   irhythm[], iinstr[], idata[], kbpm, klen [, kmode] [, kstep] [, kreset] [, kverbose]
-- >         
-- > kres   sequ   irhythm[], iinstr[],
-- >         idata[][], kbpm, klen [, kmode] [, kstep] [, kreset] [, kverbose]
-- >         
--
-- csound doc: <https://csound.com/docs/manual/sequ.html>
sequ ::  D -> Sig
sequ b1 =
  Sig $ f <$> unD b1
  where
    f a1 = opcs "sequ" [(Kr,[Ir,Ir,Ir,Kr,Kr,Kr,Kr,Kr,Kr]),(Kr,[Ir,Ir,Ir,Kr,Kr,Kr,Kr,Kr,Kr])] [a1]

-- | 
-- Configurable slider controls for realtime user input.
--
-- Configurable slider controls for realtime user input. Requires Winsound or TCL/TK. setctrl sets a slider to a specific value, or sets a minimum or maximum range.
--
-- >  setctrl  inum, ival, itype
--
-- csound doc: <https://csound.com/docs/manual/setctrl.html>
setctrl ::  D -> D -> D -> SE ()
setctrl b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "setctrl" [(Xr,[Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Sets the playback position of the current score performance to a given position.
--
-- >   setscorepos  ipos
--
-- csound doc: <https://csound.com/docs/manual/setscorepos.html>
setscorepos ::  D -> SE ()
setscorepos b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "setscorepos" [(Xr,[Ir])] [a1]

-- | 
-- Split a trigger signal
--
-- splitrig splits a trigger signal (i.e. a timed sequence of control-rate impulses) into several channels following a structure designed by the user.
--
-- >  splitrig  ktrig, kndx, imaxtics, ifn, kout1 [,kout2,...,koutN]
--
-- csound doc: <https://csound.com/docs/manual/splitrig.html>
splitrig ::  Sig -> Sig -> D -> Tab -> [Sig] -> SE ()
splitrig b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unD) b3 <*> (lift . unTab) b4 <*> mapM (lift . unSig) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "splitrig" [(Xr,[Kr,Kr,Ir,Ir] ++ (repeat Kr))] ([a1,a2,a3,a4] ++ a5)

-- | 
-- Estimate the tempo of beat patterns in a control signal.
--
-- > ktemp  tempest  kin, iprd, imindur, imemdur, ihp, ithresh, ihtim, ixfdbak, \
-- >           istartempo, ifn [, idisprd] [, itweek]
--
-- csound doc: <https://csound.com/docs/manual/tempest.html>
tempest ::  Sig -> D -> D -> D -> D -> D -> D -> D -> D -> Tab -> Sig
tempest b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 =
  Sig $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unD b6 <*> unD b7 <*> unD b8 <*> unD b9 <*> unTab b10
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = opcs "tempest" [(Kr
                                                       ,[Kr,Ir,Ir,Ir,Ir,Ir,Ir,Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10]

-- | 
-- Apply tempo control to an uninterpreted score.
--
-- >  tempo  ktempo, istartempo
--
-- csound doc: <https://csound.com/docs/manual/tempo.html>
tempo ::  Sig -> D -> SE ()
tempo b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "tempo" [(Xr,[Kr,Ir])] [a1,a2]

-- | 
-- Reads the current value of the tempo.
--
-- > kres  tempoval 
--
-- csound doc: <https://csound.com/docs/manual/tempoval.html>
tempoval ::   Sig
tempoval  =
  Sig $ return $ f 
  where
    f  = opcs "tempoval" [(Kr,[])] []

-- | 
-- Time Variant Sequencer
--
-- An event-sequencer in which time can be controlled by a
--     time-pointer. Sequence data are stored into a table.
--
-- > ktrig   timedseq   ktimpnt, ifn, kp1 [,kp2, kp3, ...,kpN]
--
-- csound doc: <https://csound.com/docs/manual/timedseq.html>
timedseq ::  Sig -> Tab -> [Sig] -> Sig
timedseq b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unTab b2 <*> mapM unSig b3
  where
    f a1 a2 a3 = opcs "timedseq" [(Kr,[Kr,Ir] ++ (repeat Kr))] ([a1,a2] ++ a3)

-- | 
-- Informs when a krate signal crosses a threshold.
--
-- > kout  trigger  ksig, kthreshold, kmode
--
-- csound doc: <https://csound.com/docs/manual/trigger.html>
trigger ::  Sig -> Sig -> Sig -> Sig
trigger b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "trigger" [(Kr,[Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Accepts a trigger signal as input and outputs a group of values.
--
-- >  trigseq  ktrig_in, kstart, kloop, kinitndx, kfn_values, kout1 [, kout2] [...]
--
-- csound doc: <https://csound.com/docs/manual/trigseq.html>
trigseq ::  Sig -> Sig -> Sig -> Sig -> Tab -> [Sig] -> SE ()
trigseq b1 b2 b3 b4 b5 b6 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> (lift . unTab) b5 <*> mapM (lift . unSig) b6
  where
    f a1 a2 a3 a4 a5 a6 = opcsDep_ "trigseq" [(Xr,(repeat Kr))] ([a1,a2,a3,a4,a5] ++ a6)

-- | 
-- Envelope follower unit generator.
--
-- Envelope follower unit generator emmulating a Perkin Elmer
--       Vactrole VTL5C3/2.
--
-- > ares  vactrol  asig [iup, idown]
--
-- csound doc: <https://csound.com/docs/manual/vactrol.html>
vactrol ::  Sig -> Sig
vactrol b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "vactrol" [(Ar,[Ar,Ir,Ir])] [a1]

-- | 
-- Reads data from a number of external Nintendo Wiimote controllers.
--
-- Opens and at control-rate polls up to four external Nintendo Wiimote controllers.
--
-- > ires  wiiconnect  [itimeout, imaxnum]
--
-- csound doc: <https://csound.com/docs/manual/wiiconnect.html>
wiiconnect ::   D
wiiconnect  =
  D $ return $ f 
  where
    f  = opcs "wiiconnect" [(Ir,[Ir,Ir])] []

-- | 
-- Reads data fields from a number of external Nintendo Wiimote controllers.
--
-- Reads data fields from upto four external Nintendo Wiimote controllers.
--
-- > kres  wiidata  kcontrol[, knum]
--
-- csound doc: <https://csound.com/docs/manual/wiidata.html>
wiidata ::  Sig -> Sig
wiidata b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "wiidata" [(Kr,[Kr,Kr])] [a1]

-- | 
-- Sets scaling and range limits for certain Wiimote fields.
--
-- >   wiirange  icontrol, iminimum, imaximum[, inum]
--
-- csound doc: <https://csound.com/docs/manual/wiirange.html>
wiirange ::  D -> D -> D -> SE ()
wiirange b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "wiirange" [(Xr,[Ir,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Sends data to one of a number of external Nintendo Wiimote controllers.
--
-- > kres  wiisend  kcontrol, kvalue[, knum]
--
-- csound doc: <https://csound.com/docs/manual/wiisend.html>
wiisend ::  Sig -> Sig -> Sig
wiisend b1 b2 =
  Sig $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = opcs "wiisend" [(Kr,[Kr,Kr,Kr])] [a1,a2]

-- | 
-- writes a value into the scratchpad of the instance of an instrument.
--
-- The writescratch opcode writes one of four
--       scalar values to be stored in the instance of an instrument.
--
-- >  writescratch ival[, index]
--
-- csound doc: <https://csound.com/docs/manual/writescratch.html>
writescratch ::  D -> SE ()
writescratch b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "writescratch" [(Xr,[Ir,Ir])] [a1]

-- | 
-- Sense the cursor position in an output window
--
-- Sense the cursor position in an output window. When xyin is called the position of the mouse within the output window is used to reply to the request.  This simple mechanism does mean that only one xyin can be used accurately at once.  The position of the mouse is reported in the output window.
--
-- > kx, ky  xyin  iprd, ixmin, ixmax, iymin, iymax [, ixinit] [, iyinit]
--
-- csound doc: <https://csound.com/docs/manual/xyin.html>
xyin ::  D -> D -> D -> D -> D -> (Sig,Sig)
xyin b1 b2 b3 b4 b5 =
  pureTuple $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5
  where
    f a1 a2 a3 a4 a5 = mopcs "xyin" ([Kr,Kr],[Ir,Ir,Ir,Ir,Ir,Ir,Ir]) [a1,a2,a3,a4,a5]

-- Stacks.

-- | 
-- Pops values from the global stack.  Deprecated.
--
-- Pops values from the global stack.
--
-- > xval1, [xval2, ... , xval31]  pop 
-- > ival1, [ival2, ... , ival31]  pop 
--
-- csound doc: <https://csound.com/docs/manual/pop.html>
pop :: forall a . Tuple a =>  a
pop  =
  pureTuple $ return $ f 
  where
    f  = mopcs "pop" ((repeat Ir),[]) []

-- | 
-- Pops an f-sig frame from the global stack.  Deprecated.
--
-- Pops an f-sig frame from the global stack.
--
-- > fsig  pop_f 
--
-- csound doc: <https://csound.com/docs/manual/pop_f.html>
pop_f ::   Spec
pop_f  =
  Spec $ return $ f 
  where
    f  = opcs "pop_f" [(Fr,[])] []

-- | 
-- Pushes a value into the global stack.  Deprecated.
--
-- Pushes a value into the global stack.
--
-- >  push   xval1, [xval2, ... , xval31]
-- >  push   ival1, [ival2, ... , ival31]
--
-- csound doc: <https://csound.com/docs/manual/push.html>
push ::  [Sig] -> SE ()
push b1 =
  SE $ join $ f <$> mapM (lift . unSig) b1
  where
    f a1 = opcsDep_ "push" [(Xr,(repeat Xr))] a1

-- | 
-- Pushes an f-sig frame into the global stack.  Deprecated.
--
-- Pushes an f-sig frame into the global stack.
--
-- >  push_f   fsig
--
-- csound doc: <https://csound.com/docs/manual/push_f.html>
push_f ::  Spec -> SE ()
push_f b1 =
  SE $ join $ f <$> (lift . unSpec) b1
  where
    f a1 = opcsDep_ "push_f" [(Xr,[Fr])] [a1]

-- | 
-- Initializes the stack.  Deprecated.
--
-- Initializes and sets the size of the global stack.
--
-- >  stack   iStackSize
--
-- csound doc: <https://csound.com/docs/manual/stack.html>
stack ::  D -> SE ()
stack b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "stack" [(Xr,[Ir])] [a1]

-- Subinstrument Control.

-- | 
-- Creates and runs a numbered instrument instance.
--
-- Creates an instance of another instrument and is used as if it were an opcode.
--
-- > a1, [...] [, a8]  subinstr  instrnum [, p4] [, p5] [...]
-- > a1, [...] [, a8]  subinstr  "insname" [, p4] [, p5] [...]
--
-- csound doc: <https://csound.com/docs/manual/subinstr.html>
subinstr :: forall a . Tuple a => D -> [D] -> a
subinstr b1 b2 =
  pureTuple $ f <$> unD b1 <*> mapM unD b2
  where
    f a1 a2 = mopcs "subinstr" ((repeat Ar),[Sr] ++ (repeat Ir)) ([a1] ++ a2)

-- | 
-- Creates and runs a numbered instrument instance at init-time.
--
-- Same as subinstr, but init-time only and has no output arguments.
--
-- >  subinstrinit  instrnum [, p4] [, p5] [...]
-- >  subinstrinit  "insname" [, p4] [, p5] [...]
--
-- csound doc: <https://csound.com/docs/manual/subinstrinit.html>
subinstrinit ::  D -> [D] -> SE ()
subinstrinit b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> mapM (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "subinstrinit" [(Xr,(repeat Ir))] ([a1] ++ a2)

-- Time Reading.

-- | 
-- Returns the number seconds since a base date.
--
-- Returns the number seconds since a base date, using the
--       operating system's clock.  The base is 1 January 1970 for Csound
--       using doubles, and 1 January 2010 for versions using floats.
--       On operating systemms with sufficient resolution the date
--       includes fractional seconds.
--
-- > ir[, inano]  date 
-- > kr[, knano]  date 
--
-- csound doc: <https://csound.com/docs/manual/date.html>
date :: forall a . Tuple a =>  a
date  =
  pureTuple $ return $ f 
  where
    f  = mopcs "date" ([Kr,Kr],[]) []

-- | 
-- Returns as a string the date and time specified.
--
-- > Sir  dates  [ itime]
--
-- csound doc: <https://csound.com/docs/manual/dates.html>
dates ::   Str
dates  =
  Str $ return $ f 
  where
    f  = opcs "dates" [(Sr,[Ir])] []

-- | 

--
-- > ires  elapsedcycles 
-- > kres  elapsedcycles 
--
-- csound doc: <https://csound.com/docs/manual/elapsedcycles.html>
elapsedcycles ::   Sig
elapsedcycles  =
  Sig $ return $ f 
  where
    f  = opcs "elapsedcycles" [(Ir,[]),(Kr,[])] []

-- | 

--
-- > ires  elapsedtime 
-- > kres  elapsedtime 
--
-- csound doc: <https://csound.com/docs/manual/elapsedtime.html>
elapsedtime ::   Sig
elapsedtime  =
  Sig $ return $ f 
  where
    f  = opcs "elapsedtime" [(Ir,[]),(Kr,[])] []

-- | 

--
-- > kres  eventcycles 
--
-- csound doc: <https://csound.com/docs/manual/eventcycles.html>
eventcycles ::   Sig
eventcycles  =
  Sig $ return $ f 
  where
    f  = opcs "eventcycles" [(Kr,[])] []

-- | 

--
-- > kres  eventtime 
--
-- csound doc: <https://csound.com/docs/manual/eventtime.html>
eventtime ::   Sig
eventtime  =
  Sig $ return $ f 
  where
    f  = opcs "eventtime" [(Kr,[])] []

-- | 
-- Reads the value of an internal clock.
--
-- > ir  readclock  inum
--
-- csound doc: <https://csound.com/docs/manual/readclock.html>
readclock ::  D -> D
readclock b1 =
  D $ f <$> unD b1
  where
    f a1 = opcs "readclock" [(Ir,[Ir])] [a1]

-- | 
-- Read the real time clock from the operating system.
--
-- Read the real-time clock from the operating system.
--
-- > ires  rtclock 
-- > kres  rtclock 
--
-- csound doc: <https://csound.com/docs/manual/rtclock.html>
rtclock ::   Sig
rtclock  =
  Sig $ return $ f 
  where
    f  = opcs "rtclock" [(Ir,[]),(Kr,[])] []

-- | 
-- Read absolute time in k-rate cycles.
--
-- Read absolute time, in k-rate cycles, since the start of an
--     	instance of an instrument. Called at both i-time as well as
--     	k-time.
--
-- > kres  timeinstk 
--
-- csound doc: <https://csound.com/docs/manual/timeinstk.html>
timeinstk ::   Sig
timeinstk  =
  Sig $ return $ f 
  where
    f  = opcs "timeinstk" [(Kr,[])] []

-- | 
-- Read absolute time in seconds.
--
-- Read absolute time, in seconds, since the start of an instance of an instrument.
--
-- > kres  timeinsts 
--
-- csound doc: <https://csound.com/docs/manual/timeinsts.html>
timeinsts ::   Sig
timeinsts  =
  Sig $ return $ f 
  where
    f  = opcs "timeinsts" [(Kr,[])] []

-- | 
-- Read absolute time in k-rate cycles.
--
-- Read absolute time, in k-rate cycles, since the start of the performance.
--
-- > ires  timek 
-- > kres  timek 
--
-- csound doc: <https://csound.com/docs/manual/timek.html>
timek ::   SE Sig
timek  =
  fmap ( Sig . return) $ SE $ join $ return $ f 
  where
    f  = opcsDep "timek" [(Ir,[]),(Kr,[])] []

-- | 
-- Read absolute time in seconds.
--
-- Read absolute time, in seconds, since the start of the performance.
--
-- > ires  times 
-- > kres  times 
--
-- csound doc: <https://csound.com/docs/manual/times.html>
times ::   SE Sig
times  =
  fmap ( Sig . return) $ SE $ join $ return $ f 
  where
    f  = opcsDep "times" [(Ir,[]),(Kr,[])] []