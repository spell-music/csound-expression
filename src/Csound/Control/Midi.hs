-- | Midi.
module Csound.Control.Midi(
    MidiChn(..), MidiFun, toMidiFun, toMidiFun_, 
    Msg, Channel, midi, midin, pgmidi, ampCps,
    midi_, midin_, pgmidi_,
    -- * Mono-midi synth
    monoMsg, holdMsg, 
    -- * Midi event streams
    midiKeyOn, midiKeyOff,
    -- * Reading midi note parameters
    cpsmidi, ampmidi, initc7, ctrl7, midiCtrl7, midiCtrl, umidiCtrl,      

    -- * Overload
    MidiInstr(..)
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
monoMsg chn portTime relTime = do
	(amp, cps, status) <- genAmpCpsSig (toMidiFun chn)
	return (port amp portTime * port status relTime,  port cps portTime)

-- | Produces midi amplitude and frequency as a signal and holds the 
-- last value till the next one is present.
-- It can be used in mono-synths. Arguments are portamento time
-- and release time. A portamento time is time it takes for transition
-- from one note to another.
--
-- > holdMsg portamentoTime
holdMsg :: MidiChn -> D -> SE (Sig, Sig)
holdMsg channel portTime = do
	(amp, cps) <- genHoldAmpCpsSig (toMidiFun_ channel)
	return (port amp portTime,  port cps portTime)


genAmpCpsSig :: ((Msg -> SE Sig) -> SE Sig) -> SE (Sig, Sig, Sig)
genAmpCpsSig midiFun = do
	ref <- newGlobalSERef ((0, 0) :: (Sig, Sig))
	status <- midiFun (instr ref)
	let resStatus = ifB (downsamp status ==* 0) 0 1
	(amp, cps) <- readSERef ref
	return (downsamp amp, downsamp cps, resStatus)
	where 
		instr :: SERef (Sig, Sig) -> Msg -> SE Sig
		instr hNote msg = do
			writeSERef hNote (sig $ ampmidi msg 1, sig $ cpsmidi msg)
			return 1		

genHoldAmpCpsSig :: ((Msg -> SE ()) -> SE ()) -> SE (Sig, Sig)
genHoldAmpCpsSig midiFun = do
	ref <- newGlobalSERef ((0, 0) :: (Sig, Sig))
	midiFun (instr ref)	
	(amp, cps) <- readSERef ref
	return (downsamp amp, downsamp cps)
	where 
		instr :: SERef (Sig, Sig) -> Msg -> SE ()
		instr hNote msg = do
			writeSERef hNote (sig $ ampmidi msg 1, sig $ cpsmidi msg)			


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
	chRef  <- newGlobalSERef (0 :: Sig)
	evtRef <- newGlobalSERef (0 :: Sig)
	writeSERef chRef =<< midiFun instr

	alwaysOn $ do
		a <- readSERef chRef
		writeSERef evtRef $ diff a

	evtSig <- readSERef evtRef
	return $ filterE ( >* 0) $ snaps evtSig
	where
		instr msg = do
			print' [notnum msg] 
			return $ ifB (boolSig $ notnum msg ==* key) (sig $ ampmidi msg 1) 0


midiKeyOffBy :: MidiFun Sig -> D -> SE Tick
midiKeyOffBy midiFun key = do	
	chRef  <- newGlobalSERef (0 :: Sig)
	evtRef <- newGlobalSERef (0 :: Sig)
	writeSERef chRef =<< midiFun instr

	alwaysOn $ do
		a <- readSERef chRef
		writeSERef evtRef $ diff a

	evtSig <- readSERef evtRef
	return $ fmap (const unit) $ filterE ( <* 0) $ snaps evtSig
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

