-- | Midi.
module Csound.Control.Midi(
    Msg, Channel, midi, midin, pgmidi, ampCps,
    midi_, midin_, pgmidi_,
    -- * Mono-midi synth
    monoMsg, holdMsg, monoMsgn, holdMsgn, pgmonoMsg, pgholdMsg,
    -- * Reading midi note parameters
    cpsmidi, ampmidi, initc7, ctrl7, midiCtrl7, midiCtrl, umidiCtrl,        
    -- * Overload
    MidiInstr(..)
) where

import Data.Boolean

import Csound.Typed
import Csound.Typed.Opcode hiding (initc7)
import Csound.Control.Overload

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
-- > monoMsg portamentoTime releaseTime
monoMsg :: D -> D -> SE (Sig, Sig)
monoMsg portTime relTime = do
	(amp, cps, status) <- genAmpCpsSig midi
	return (port amp portTime * port status relTime,  port cps portTime)

-- | Produces midi amplitude and frequency as a signal and holds the 
-- last value till the next one is present.
-- It can be used in mono-synths. Arguments are portamento time
-- and release time. A portamento time is time it takes for transition
-- from one note to another.
--
-- > holdMsg portamentoTime
holdMsg :: D -> SE (Sig, Sig)
holdMsg portTime = do
	(amp, cps) <- genHoldAmpCpsSig midi_
	return (port amp portTime,  port cps portTime)


-- | Produces midi amplitude and frequency as a signal.
-- The signal fades out when nothing is pressed. We can specify a channel.
-- It can be used in mono-synths. Arguments are portamento time
-- and release time. A portamento time is time it takes for transition
-- from one note to another.
--
-- > monoMsgn chnNumber portamentoTime releaseTime
monoMsgn :: Channel -> D -> D -> SE (Sig, Sig)
monoMsgn n portTime relTime = do
	(amp, cps, status) <- genAmpCpsSig (midin n)
	return (port amp portTime * port status relTime,  port cps portTime)

-- | Produces midi amplitude and frequency as a signal and holds the 
-- last value till the next one is present. We can specify a channel.
-- It can be used in mono-synths. Arguments are portamento time
-- and release time. A portamento time is time it takes for transition
-- from one note to another.
--
-- > holdMsgn chnNumber portamentoTime
holdMsgn :: Channel -> D -> SE (Sig, Sig)
holdMsgn n portTime = do
	(amp, cps) <- genHoldAmpCpsSig (midin_ n)
	return (port amp portTime,  port cps portTime)


-- | Produces midi amplitude and frequency as a signal.
-- The signal fades out when nothing is pressed. We can specify a programm number and channel.
-- It can be used in mono-synths. Arguments are portamento time
-- and release time. A portamento time is time it takes for transition
-- from one note to another.
--
-- > pgmonoMsg chnNumber portamentoTime releaseTime
pgmonoMsg :: Maybe Int -> Channel -> D -> D -> SE (Sig, Sig)
pgmonoMsg pg n portTime relTime = do
	(amp, cps, status) <- genAmpCpsSig (pgmidi pg n)
	return (port amp portTime * port status relTime,  port cps portTime)

-- | Produces midi amplitude and frequency as a signal and holds the 
-- last value till the next one is present. We can specify a programm number and channel.
-- It can be used in mono-synths. Arguments are portamento time
-- and release time. A portamento time is time it takes for transition
-- from one note to another.
--
-- > pgholdMsg portamentoTime
pgholdMsg :: Maybe Int -> Channel -> D -> SE (Sig, Sig)
pgholdMsg pg n portTime = do
	(amp, cps) <- genHoldAmpCpsSig (pgmidi_ pg n)
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

