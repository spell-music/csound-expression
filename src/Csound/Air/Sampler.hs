module Csound.Air.Sampler (

	-- * Event sampler

	-- | Note: The release phase of the instrument is skipped
	-- with event sampler functions.
	evtTrig, evtTap, evtGroup, evtCycle,

	-- * Keyboard sampler
	charTrig, charTap, charPush, charToggle, charGroup, charCycle,

    -- * Midi sampler
    midiTrig, midiTap, midiPush, midiToggle, midiGroup, 

    -- * Generic functions
    midiTrigBy, midiTapBy, midiPushBy, midiToggleBy, midiGroupBy,

    -- ** Midi instruments
    MidiTrigFun, midiAmpInstr, midiLpInstr, midiAudioLpInstr, midiConstInstr
) where

import Data.Monoid
import Data.Boolean
import Temporal.Class

import Csound.Typed
import Csound.Control
import Csound.SigSpace

import Csound.Air.Filter(mlp)
import Csound.Air.Wav(takeSnd)
import Csound.Air.Seg

-----------------------------------------------------------
-- Event sampler

-- | Triggers the signal with the first stream and turns it off with the second stream.
evtTrig :: (Sigs a) => Tick -> Tick -> a -> a
evtTrig x st a = runSeg $ loop $ lim st $ del x $ loop (lim x $ toSeg a)

-- | Toggles the signal with event stream.
evtToggle :: (Sigs a) => Tick -> a -> a
evtToggle evt = evtTrig (fmap (const unit) ons) (fmap (const unit) offs)
	where (offs, ons) = splitToggle $ toTog evt

-- | Consider note limiting? or performance degrades
-- every note is held to infinity and it continues to produce zeroes.
-- No it's not every sequence note triggers it
-- but it's best to limit them anyway
evtTap :: (Sigs a) => D -> Tick -> a -> a
evtTap dt x a = runSeg $ del x $ loop $ lim x $ toSeg $ takeSnd dt a

-- | Plays a list signals. It triggers the signal with event stream and silences
-- all the rest in the list so that only one signal is playing. We can create simple
-- costum monosynthes with this function. The last event stream stops all signals.
evtGroup :: (Sigs a) => [(Tick, a)] -> Tick -> a
evtGroup as stop = sum $ fmap (\(a, b, c) -> evtTrig a (mappend b stop) c) 
	$ zipWith (\n (a, sam) -> (a, mconcat $ fmap snd $ filter ((/= n) . fst) allEvts, sam)) [(0 :: Int)..] as
	where 
		allEvts :: [(Int, Tick)]
		allEvts = zip [0 ..] (fmap fst as) 

-- | Triggers one signal after another with an event stream.
evtCycle :: (Sigs a) => Tick -> Tick -> [a] -> a
evtCycle start stop sigs = runSeg $ loop $ lim stop $ del start $ loop $ mel $ fmap (lim start . toSeg) sigs

-----------------------------------------------------------
-- Char sampler

-- | Triggers a signal when one of the chars from the first string is pressed.
-- Stos signal from playing when one of the chars from the second string is pressed.
charTrig :: (Sigs a) => String -> String -> a -> a
charTrig starts stops asig = runSeg $ loop $ lim (strOn stops) $ toSeg $ retrig (const $ return asig) (strOn starts)

-- | Plays a signal while a key is pressed.
charPush :: Sigs a => Char -> a -> a
charPush ch = evtTrigger (charOn ch) (charOff ch)

-- | Toggles the signal when key is pressed.
charToggle :: (Sigs a) => Char -> a -> a
charToggle key asig = retrig (togInstr asig) 
	$ accumE (1 :: D) (\_ s -> (s, mod' (s + 1) 2)) 
	$ charOn key
	where 
		togInstr asig isPlay = do
			ref <- newSERef 0
			when1 (sig isPlay ==* 1) $ do
				writeSERef ref asig
			readSERef ref

-- | Consider note limiting? or performance degrades
-- every note is held to infinity and it continues to produce zeroes.
-- No it's not every sequence note triggers it
-- but it's best to limit them anyway
charTap :: Sigs a => D -> String -> a -> a
charTap stop starts = evtTap stop (strOn starts)

-- | Plays a list of signals when corresponding key is pressed.
-- Turns off all other signals in the group. The last string is
-- for stopping the group from playing.
charGroup :: (Sigs a) => [(Char, a)] -> String -> a
charGroup as stop = sum $ fmap f as
	where 
		allKeys = fmap fst as ++ stop
		f (key, asig) = evtTrigger ons offs asig
			where
				ons  = charOn key
				offs = strOn allKeys			

-- | Plays signals one after another when key is pressed.
-- Stops the group from playing when the char from the last 
-- argument is pressed.
charCycle :: Sigs a => Char -> String -> [a] -> a
charCycle start stop = evtCycle (charOn start) (strOn stop) 

---------------------------------------------------------------------

evtTrigger :: (Sigs a) => Tick -> Tick -> a -> a
evtTrigger ons offs asig = schedUntil (const $ return asig) ons offs

----------------------------------------------------------
-- Midi sampler

type MidiTrigFun a = a -> D -> SE a

-- | Scales the signal with the amplitude.
midiAmpInstr :: (SigSpace a, Sigs a) => a -> D -> SE a
midiAmpInstr asig amp = return $ mul (sig amp) asig

-- | Applies a low pass filter to the signal.
-- The first two arguments are the frequency range for center frequency of the filter
-- and the second one is amount of resonance (ranges from 0 to 1).
midiLpInstr :: (SigSpace a, Sigs a) => (Sig, Sig) -> Sig -> a -> D -> SE a
midiLpInstr (minC, maxC) q asig amp = return $ mapSig (mlp (minC * ((maxC / minC) ** sig amp) ) q) asig

-- | the midiLpInstr with audio range for center frequency.
midiAudioLpInstr :: (SigSpace a, Sigs a) => Sig -> a -> D -> SE a
midiAudioLpInstr = midiLpInstr (50, 10000)

-- | Ignores the amplitude and justplays back the original signal.
midiConstInstr :: (SigSpace a, Sigs a) => a -> D -> SE a
midiConstInstr asig amp = return asig

-- | Plays a signal when the key is pressed. Retriggers the signal when the key is pressed again.
-- The key is an integer midi code. The C1 is 60 and the A1 is 69.
midiTrig :: (SigSpace a, Sigs a) => MidiChn -> Int -> a -> SE a
midiTrig = midiTrigBy midiAmpInstr

-- | Plays a signal when the key is pressed. Retriggers the signal when the key is pressed again.
-- Turns off the signal after specified duration (n seconds).
-- The key is an integer midi code. The C1 is 60 and the A1 is 69.
midiTap :: (SigSpace a, Sigs a) => MidiChn -> D -> Int -> a -> SE a
midiTap = midiTapBy midiAmpInstr

-- | Plyas a signal while the key is pressed.
-- The key is an integer midi code. The C1 is 60 and the A1 is 69.
midiPush :: (SigSpace a, Sigs a) => MidiChn -> Int -> a -> SE a
midiPush = midiPushBy midiAmpInstr

-- | Plays and stops a signal in the toggle mode. 
-- The key is an integer midi code. The C1 is 60 and the A1 is 69.
midiToggle :: (SigSpace a, Sigs a) => MidiChn -> Int -> a -> SE a
midiToggle = midiToggleBy midiAmpInstr

-- | Plays a set of signals on the list of keys. When certain 
-- key is pressed the corresponding signal starts to play and all
-- the rest are stopped. 
--
-- -- The key is an integer midi code. The C1 is 60 and the A1 is 69.
midiGroup :: (SigSpace a, Sigs a) => MidiChn -> [(Int, a)] -> SE a
midiGroup = midiGroupBy midiAmpInstr

-- | The generic midiTrig. We can specify the midi function.
-- The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
-- It produces some output. The default is scaling the signal with the amplitude.
midiTrigBy :: (SigSpace a, Sigs a) => MidiTrigFun a -> MidiChn -> Int -> a -> SE a
midiTrigBy midiInstr midiChn key asig = fmap (\evt -> retrig (midiInstr asig) evt) (midiKeyOn midiChn $ int key)

-- | The generic midiTap. We can specify the midi function.
-- The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
-- It produces some output. The default is scaling the signal with the amplitude.
midiTapBy :: (SigSpace a, Sigs a) => MidiTrigFun a -> MidiChn -> D -> Int -> a -> SE a
midiTapBy midiInstr midiChn dt key asig = midiTrigBy midiInstr midiChn key (takeSnd dt asig)

-- | The generic midiPush. We can specify the midi function.
-- The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
-- It produces some output. The default is scaling the signal with the amplitude.
midiPushBy :: (SigSpace a, Sigs a) => MidiTrigFun a -> MidiChn -> Int -> a -> SE a
midiPushBy midiInstr midiChn key asig = do
	ons  <- midiKeyOn midiChn (int key)
	offs <- midiKeyOff midiChn (int key)
	return $ midiEvtTriggerBy midiInstr ons offs asig	

-- | The generic midiToggle. We can specify the midi function.
-- The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
-- It produces some output. The default is scaling the signal with the amplitude.
midiToggleBy :: (SigSpace a, Sigs a) => MidiTrigFun a -> MidiChn -> Int -> a -> SE a
midiToggleBy midiInstr midiChn key asig = fmap (\evt -> retrig (togMidiInstr asig) evt) 
	(fmap (accumE (1 :: D) (\a s -> ((a, s), mod' (s + 1) 2))) $ midiKeyOn midiChn $ int key)
	where 
		togMidiInstr asig (amp, isPlay) = do
			ref <- newSERef 0
			when1 (sig isPlay ==* 1) $ do
				writeSERef ref =<< midiInstr asig amp
			readSERef ref

-- | The generic midiGroup. We can specify the midi function.
-- The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
-- It produces some output. The default is scaling the signal with the amplitude.
midiGroupBy :: (SigSpace a, Sigs a) => MidiTrigFun a -> MidiChn -> [(Int, a)] -> SE a
midiGroupBy midiInstr midiChn as = fmap sum $ mapM f as
	where 
		allKeys = fmap fst as
		f (key, asig) = do
			ons  <- midiKeyOn midiChn (int key)
			offs <- fmap (fmap (const unit) . mconcat) $ mapM (midiKeyOn midiChn . int) allKeys
			return $ midiEvtTriggerBy midiInstr ons offs asig

midiEvtTriggerBy :: (SigSpace a, Sigs a) => (a -> D -> SE a) -> Evt D -> Tick -> a -> a
midiEvtTriggerBy midiInstr ons offs asig = schedUntil (midiAmpInstr asig) ons offs
