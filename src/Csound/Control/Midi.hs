{-# Language FlexibleContexts #-}
-- | Midi.
module Csound.Control.Midi(
    MidiChn(..), MidiFun, toMidiFun, toMidiFun_, 
    Msg, Channel, midi, midin, pgmidi, ampCps,
    midi_, midin_, pgmidi_,
    -- * Mono-midi synth
    monoMsg, holdMsg, trigNamedMono,
    -- * Midi event streams
    midiKeyOn, midiKeyOff,
    -- * Reading midi note parameters
    cpsmidi, ampmidi, initc7, ctrl7, midiCtrl7, midiCtrl, umidiCtrl,
    ampmidinn,

    -- * Overload
    tryMidi, MidiInstr(..)
) where

import Data.Boolean

import Csound.Typed
import Csound.Typed.Opcode hiding (initc7)
import Csound.Control.Overload
import Csound.Control.Instr(alwaysOn)
import Csound.Control.Evt(Tick)

-- | Specifies the midi channel or programm.
data MidiChn = ChnAll | Chn Int | Pgm (Maybe Int) Int
	deriving (Show, Eq)

type MidiFun a = (Msg -> SE a) -> SE a

toMidiFun :: Sigs a => MidiChn -> MidiFun a
toMidiFun x = case x of
	ChnAll  -> midi
	Chn n   -> midin n
	Pgm a b -> pgmidi a b

toMidiFun_ :: MidiChn -> MidiFun ()
toMidiFun_ x = case x of
	ChnAll  -> midi_
	Chn n   -> midin_ n
	Pgm a b -> pgmidi_ a b

ampCps :: Msg -> (D, D)
ampCps msg = (ampmidi msg 1, cpsmidi msg)

-- | Converts midi velocity number to amplitude. 
-- The first argument is dynamic range in decibels.
--
-- > ampmidinn (volMinDb, volMaxDb) volumeKey = amplitude
ampmidinn :: (D, D) -> D -> D
ampmidinn (volMin, volMax) volKey = ampdbfs (volMin + ir (ampmidid volKey (volMax - volMin)))

-----------------------------------------------------------------------
-- Midi addons

-- mono midi

-- | Produces midi amplitude and frequency as a signal.
-- The signal fades out when nothing is pressed.
-- It can be used in mono-synths. Arguments are portamento time
-- and release time. A portamento time is time it takes for transition
-- from one note to another.
--
-- > monoMsg channel portamentoTime releaseTime
monoMsg :: MidiChn -> D -> D -> SE (Sig, Sig)
monoMsg = genMonoMsg cpsmidi

genMonoMsg :: (Msg -> D) -> MidiChn -> D -> D -> SE (Sig, Sig)
genMonoMsg key2cps chn portTime relTime = do
	(amp, cps, status) <- genAmpCpsSig key2cps (toMidiFun chn)
	return (port amp portTime * port status relTime,  port cps portTime)

-- | Produces midi amplitude and frequency as a signal and holds the 
-- last value till the next one is present.
-- It can be used in mono-synths. Arguments are portamento time
-- and release time. A portamento time is time it takes for transition
-- from one note to another.
--
-- > holdMsg portamentoTime
holdMsg :: MidiChn -> D -> SE (Sig, Sig)
holdMsg = genHoldMsg cpsmidi

genHoldMsg :: (Msg -> D) -> MidiChn -> D -> SE (Sig, Sig)
genHoldMsg key2cps channel portTime = do
	(amp, cps) <- genHoldAmpCpsSig key2cps (toMidiFun_ channel)
	return (port amp portTime,  port cps portTime)


genAmpCpsSig :: (Msg -> D) -> ((Msg -> SE Sig) -> SE Sig) -> SE (Sig, Sig, Sig)
genAmpCpsSig key2cps midiFun = do
	ref <- newGlobalRef ((0, 0) :: (Sig, Sig))
	status <- midiFun (instr ref)
	let resStatus = ifB (downsamp status ==* 0) 0 1
	(amp, cps) <- readRef ref
	return (downsamp amp, downsamp cps, resStatus)
	where 
		instr :: Ref (Sig, Sig) -> Msg -> SE Sig
		instr hNote msg = do
			writeRef hNote (sig $ ampmidi msg 1, sig $ key2cps msg)
			return 1		

genHoldAmpCpsSig :: (Msg -> D) -> ((Msg -> SE ()) -> SE ()) -> SE (Sig, Sig)
genHoldAmpCpsSig key2cps midiFun = do
	ref <- newGlobalRef ((0, 0) :: (Sig, Sig))
	midiFun (instr ref)	
	(amp, cps) <- readRef ref
	return (downsamp amp, downsamp cps)
	where 
		instr :: Ref (Sig, Sig) -> Msg -> SE ()
		instr hNote msg = do
			writeRef hNote (sig $ ampmidi msg 1, sig $ key2cps msg)			

trigNamedMono :: D -> D -> String -> SE (Sig, Sig)
trigNamedMono portTime relTime name = namedMonoMsg portTime relTime name

{-	do
	(volKey, pitchKey, status) <- namedAmpCpsSig name
	return (port volKey portTime * port status relTime,  port pitchKey portTime)
-}
-- namedMonoMsg portTime relTime name

namedAmpCpsSig:: String -> SE (Sig, Sig, Sig)
namedAmpCpsSig name = do
	ref <- newGlobalRef ((0, 0) :: (Sig, Sig))	
	statusRef <- newGlobalRef (0 :: Sig)
	status <- trigByNameMidi name (instr statusRef ref)
	writeRef statusRef status 
	let resStatus = ifB (downsamp status ==* 0) 0 1
	(amp, cps) <- readRef ref
	return (downsamp amp, downsamp cps, resStatus)
	where 
		instr :: Ref Sig -> Ref (Sig, Sig) -> (D, D, Unit) -> SE Sig
		instr statusRef hNote (pitchKey, volKey, _) = do
			curId <- readRef statusRef
			myIdRef <- newRef (ir curId)
			myId <- readRef myIdRef			
			when1 (curId ==* (sig $ myId + 1)) $ do
				writeRef hNote (sig volKey, sig pitchKey)
			return 1

--------------------------------------------------------------

-- | Listens to midi on event on the given key as event stream.
-- The event stream carries the level of volume (ranges from 0 to 1).
midiKeyOn :: MidiChn -> D -> SE (Evt D)
midiKeyOn = midiKeyOnBy . toMidiFun

-- | Listens to midi on event off the given key as event stream.
midiKeyOff :: MidiChn -> D -> SE Tick
midiKeyOff = midiKeyOffBy . toMidiFun

midiKeyOnBy :: MidiFun Sig -> D -> SE (Evt D)
midiKeyOnBy midiFun key = do	
	chRef  <- newGlobalRef (0 :: Sig)
	evtRef <- newGlobalRef (0 :: Sig)
	writeRef chRef =<< midiFun instr

	alwaysOn $ do
		a <- readRef chRef
		writeRef evtRef $ diff a

	evtSig <- readRef evtRef
	return $ filterE ( >* 0) $ snaps evtSig
	where
		instr msg = do
			print' [notnum msg] 
			return $ ifB (boolSig $ notnum msg ==* key) (sig $ ampmidi msg 1) 0


midiKeyOffBy :: MidiFun Sig -> D -> SE Tick
midiKeyOffBy midiFun key = do	
	chRef  <- newGlobalRef (0 :: Sig)
	evtRef <- newGlobalRef (0 :: Sig)
	writeRef chRef =<< midiFun instr

	alwaysOn $ do
		a <- readRef chRef
		writeRef evtRef $ diff a

	evtSig <- readRef evtRef
	return $ fmap (const unit) $ filterE ( `lessThan` 0) $ snaps evtSig
	where
		instr msg = do
			print' [notnum msg] 
			return $ ifB (boolSig $ notnum msg ==* key) (sig $ ampmidi msg 1) 0

--------------------------------------------------------------

-- | Initialization of the midi control-messages.
initc7 :: D -> D -> D -> SE ()
initc7 = initMidiCtrl 

-- | Initializes midi control and get the value in the specified range.
midiCtrl7 :: D -> D -> D -> D -> D -> SE Sig
midiCtrl7 chno ctrlno ival imin imax = do
    initc7 chno ctrlno ival
    return $ ctrl7 chno ctrlno imin imax
    
-- | Initializes midi control and get the value in the range (-1) to 1.
midiCtrl :: D -> D -> D -> SE Sig
midiCtrl chno ctrlno ival = midiCtrl7 chno ctrlno ival (-1) 1
    
-- | Unipolar midiCtrl. Initializes midi control and get the value in the range 0 to 1.
umidiCtrl :: D -> D -> D -> SE Sig
umidiCtrl chno ctrlno ival = midiCtrl7 chno ctrlno ival 0 1

--------------------------------------------------------------

-- | Invokes ooverloaded instruments with midi.
-- Example:
--
-- > dac $ tryMidi (mul (fades 0.01 0.1) . tri)
tryMidi :: (MidiInstr a, Sigs (MidiInstrOut a)) => a -> SE (MidiInstrOut a)
tryMidi x = midi $ onMsg x