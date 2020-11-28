module Csound.Sam.Trig (
	-- * Char sampler
	samCharTrig, samCharTap, samCharPush, samCharToggle, samCharGroup, samCharCycle,

	-- ** Synchronized with number of beats
	samSyncCharTrig, samSyncCharPush, samSyncCharToggle, samSyncCharTap, samSyncCharGroup, samSyncCharCycle,

	-- * Midi sampler
	samMidiTrig, samMidiTap, samMidiPush, samMidiToggle, samMidiGroup,

	-- ** Generic functions
	samMidiTrigBy, samMidiTapBy, samMidiPushBy, samMidiToggleBy, samMidiGroupBy,
) where

import Data.Foldable(Foldable(foldMap))
import Data.Traversable hiding (mapM)
import Control.Arrow(first, second)

import Csound.Base
import qualified Csound.Sam.Core as S
import Csound.Sam.Core(Sam, bindSam, mapBpm, mapBpm2)

------------------------------------------------------

-- | Triggers the sample with any char from the first string
-- and stops the sample with any char from the second string.
samCharTrig :: Maybe Sam -> String -> String -> Sam -> Sam
samCharTrig initVal starts stops x = case initVal of
	Nothing -> fmap (charTrig Nothing starts stops) x
	Just v0 -> liftA2 (\v sigs -> charTrig (Just v) starts stops sigs) v0 x

-- | Plays a sample while the key is pressed.
samCharPush :: Maybe Sam -> Char -> Sam -> Sam
samCharPush initVal ch x = case initVal of
	Nothing -> fmap (charPush Nothing ch) x
	Just v0 -> liftA2 (\v sigs -> charPush (Just v) ch sigs) v0 x

-- | Toggles the sample when the key is pressed.
samCharToggle :: Maybe Sam -> Char -> Sam -> Sam
samCharToggle initVal ch x = case initVal of
	Nothing -> fmap (charToggle Nothing ch) x
	Just v0 -> liftA2 (\v sigs -> charToggle (Just v) ch sigs) v0 x

-- | Char trigger with fixed note limiting by length in second.
-- It's useful optimization. It's good to use for drum notes and short sounds.
samCharTap :: Sig -> String -> Sam -> Sam
samCharTap stop starts = fmap (charTap stop starts)

-- | Plays one of the sample from the list when corresponding char is pressed.
-- The last string is for stopping the samples.
samCharGroup :: Maybe Sam -> [(Char, Sam)] -> String -> Sam
samCharGroup initVal as stop = case initVal of
	Nothing -> fmap (\xs -> charGroup Nothing (zip starts xs) stop) (sequenceA sams)
	Just v0 -> liftA2 (\v xs -> charGroup (Just v) (zip starts xs) stop) v0 (sequenceA sams)
	where (starts, sams) = unzip as

-- | Plays samples in sequence when key is pressed. The last string is
-- for stopping the sequence.
samCharCycle :: Maybe Sam -> Char -> String -> [Sam] -> Sam
samCharCycle initVal start stop as = case initVal of
	Nothing -> fmap (charCycle Nothing start stop) (sequenceA as)
	Just v0 -> liftA2 (\v xs -> charCycle (Just v) start stop xs) v0 (sequenceA as)


------------------------------------------------------
-- synchronised

syncBeats :: Sig -> Sig -> Sig
syncBeats bpm beats = bpm / beats

-- | Triggers the sample with any char from the first string
-- and stops the sample with any char from the second string.
-- The first argument is the number of beats for syncronization.
samSyncCharTrig :: Sig -> Maybe Sam -> String -> String -> Sam -> Sam
samSyncCharTrig beats initVal starts stops x = case initVal of
	Nothing -> mapBpm (\bpm a -> syncCharTrig (syncBeats bpm beats) Nothing starts stops a) x
	Just v0 -> mapBpm2 (\bpm v sigs -> syncCharTrig (syncBeats bpm beats) (Just v) starts stops sigs) v0 x

-- | Plays a sample while the key is pressed.
-- The first argument is the number of beats for syncronization.
samSyncCharPush :: Sig -> Maybe Sam -> Char -> Sam -> Sam
samSyncCharPush beats initVal ch x = case initVal of
	Nothing -> mapBpm (\bpm a -> syncCharPush (syncBeats bpm beats) Nothing ch a) x
	Just v0 -> mapBpm2 (\bpm v sigs -> syncCharPush (syncBeats bpm beats) (Just v) ch sigs) v0 x

-- | Toggles the sample when the key is pressed.
-- The first argument is the number of beats for syncronization.
samSyncCharToggle :: Sig -> Maybe Sam -> Char -> Sam -> Sam
samSyncCharToggle beats initVal ch x = case initVal of
	Nothing -> mapBpm (\bpm a -> syncCharToggle (syncBeats bpm beats) Nothing ch a) x
	Just v0 -> mapBpm2 (\bpm v sigs -> syncCharToggle (syncBeats bpm beats) (Just v) ch sigs) v0 x

-- | Char trigger with fixed note limiting by length in second.
-- It's useful optimization. It's good to use for drum notes and short sounds.
-- The first argument is the number of beats for syncronization.
samSyncCharTap :: Sig -> Sig -> String -> Sam -> Sam
samSyncCharTap beats stop starts = mapBpm (\bpm x -> syncCharTap (syncBeats bpm beats) stop starts x)

-- | Plays one of the sample from the list when corresponding char is pressed.
-- The last string is for stopping the samples.
samSyncCharGroup :: Sig -> Maybe Sam -> [(Char, Sam)] -> String -> Sam
samSyncCharGroup beats initVal as stop = case initVal of
	Nothing -> mapBpm (\bpm xs -> syncCharGroup (syncBeats bpm beats) Nothing (zip starts xs) stop) (sequenceA sams)
	Just v0 -> mapBpm2 (\bpm v xs -> syncCharGroup (syncBeats bpm beats) (Just v) (zip starts xs) stop) v0 (sequenceA sams)
	where (starts, sams) = unzip as

-- | Plays samples in sequence when key is pressed. The last string is
-- for stopping the sequence.
-- The first argument is the number of beats for syncronization.
samSyncCharCycle :: Sig -> Maybe Sam -> Char -> String -> [Sam] -> Sam
samSyncCharCycle beats initVal start stop as = case initVal of
	Nothing -> mapBpm (\bpm -> syncCharCycle (syncBeats bpm beats) Nothing start stop) (sequenceA as)
	Just v0 -> mapBpm2 (\bpm v xs -> syncCharCycle (syncBeats bpm beats) (Just v) start stop xs) v0 (sequenceA as)

------------------------------------------------------

-- | Triggers a sample with midi key.
-- The key is an integer midi code. The C1 is 60 and the A1 is 69.
samMidiTrig :: MidiChn -> Int -> Sam -> Sam
samMidiTrig = samMidiTrigBy midiAmpInstr

-- | Midi trigger with fixed note limiting by length in second.
-- It's useful optimization. It's good to use for drum notes and short sounds.
-- The key is an integer midi code. The C1 is 60 and the A1 is 69.
samMidiTap :: MidiChn -> Sig -> Int -> Sam -> Sam
samMidiTap = samMidiTapBy midiAmpInstr

samMidiPush :: MidiChn -> Int -> Sam -> Sam
samMidiPush = samMidiPushBy midiAmpInstr

-- | Toggles samples with midi key.
-- The key is an integer midi code. The C1 is 60 and the A1 is 69.
samMidiToggle :: MidiChn -> Int -> Sam -> Sam
samMidiToggle = samMidiToggleBy midiAmpInstr

-- | Plays samples in the group. It's like the samCharGroup.
-- The key is an integer midi code. The C1 is 60 and the A1 is 69.
samMidiGroup :: MidiChn -> [(Int, Sam)] -> Sam
samMidiGroup midiChn as = S.liftSam $ fmap (\xs -> midiGroup midiChn $ zip keys xs) $ sequenceA sams
	where (keys, sams) = unzip as

-- | Generic samMidiTrig. We can specify the midi triggering function.
-- The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
-- It produces some output. The default is scaling the signal with the amplitude.
samMidiTrigBy :: MidiTrigFun Sig2 -> MidiChn -> Int -> Sam -> Sam
samMidiTrigBy midiFun midiChn key = bindSam (midiTrigBy midiFun midiChn key)

-- | Generic samMidiTap. We can specify the midi triggering function.
-- The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
-- It produces some output. The default is scaling the signal with the amplitude.
samMidiTapBy :: MidiTrigFun Sig2 -> MidiChn -> Sig -> Int -> Sam -> Sam
samMidiTapBy midiFun midiChn dt key = bindSam (midiTapBy midiFun midiChn dt key)

-- | Generic samMidiPush. We can specify the midi triggering function.
-- The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
-- It produces some output. The default is scaling the signal with the amplitude.
samMidiPushBy :: MidiTrigFun Sig2 -> MidiChn -> Int -> Sam -> Sam
samMidiPushBy midiFun midiChn key = bindSam (midiPushBy midiFun midiChn key)

-- | Generic samMidiToggle. We can specify the midi triggering function.
-- The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
-- It produces some output. The default is scaling the signal with the amplitude.
samMidiToggleBy :: MidiTrigFun Sig2 -> MidiChn -> Int -> Sam -> Sam
samMidiToggleBy midiFun midiChn key = bindSam (midiToggleBy midiFun midiChn key)

-- | Generic samMidiGroup. We can specify the midi triggering function.
-- The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
-- It produces some output. The default is scaling the signal with the amplitude.
samMidiGroupBy :: MidiTrigFun Sig2 -> MidiChn -> [(Int, Sam)] -> Sam
samMidiGroupBy  midiFun midiChn as = S.liftSam $ fmap (\xs -> midiGroupBy midiFun midiChn $ zip keys xs) $ sequenceA sams
	where (keys, sams) = unzip as