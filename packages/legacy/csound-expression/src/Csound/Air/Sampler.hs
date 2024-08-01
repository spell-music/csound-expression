module Csound.Air.Sampler (
  -- * Event sampler

  -- | Note: The release phase of the instrument is skipped
  -- with event sampler functions.
  evtTrig,
  evtTap,
  evtGroup,
  evtCycle,
  syncEvtTrig,
  syncEvtTap,
  syncEvtGroup,
  syncEvtCycle,

  -- * Keyboard sampler
  charTrig,
  charTap,
  charPush,
  charToggle,
  charGroup,
  charCycle,
  syncCharTrig,
  syncCharTap,
  syncCharPush,
  syncCharToggle,
  syncCharGroup,
  syncCharCycle,
  syncEvtToggle,

  -- * Midi sampler
  midiTrig,
  midiTap,
  midiPush,
  midiToggle,
  midiGroup,

  -- * Generic functions
  midiTrigBy,
  midiTapBy,
  midiPushBy,
  midiToggleBy,
  midiGroupBy,

  -- ** Midi instruments
  MidiTrigFun,
  midiAmpInstr,
  midiLpInstr,
  midiAudioLpInstr,
  midiConstInstr,

  -- * Misc

  -- | Keyboard char columns
  keyColumn1,
  keyColumn2,
  keyColumn3,
  keyColumn4,
  keyColumn5,
  keyColumn6,
  keyColumn7,
  keyColumn8,
  keyColumn9,
  keyColumn0,
  keyColumns,
) where

import Data.Boolean
import Temporal.Class

import Csound.Control
import Csound.Typed

import Csound.Air.Filter (mlp)
import Csound.Air.Seg
import Csound.Air.Wav (takeSnd)

-----------------------------------------------------------
-- Event sampler

-- | Triggers the signal with the first stream and turns it off with the second stream.
evtTrig :: (Sigs a) => Maybe a -> Tick -> Tick -> a -> a
evtTrig minitVal x st a = case minitVal of
  Nothing -> ons
  Just v0 -> ons + offs v0 + first v0
  where
    ons = evtTrigNoInit x st a
    offs v = evtTrigNoInit st x v
    first v = evtTrigger loadbang x v

    evtTrigNoInit xEvt stEvt aSig = runSeg $ loop $ lim stEvt $ del xEvt $ loop (lim xEvt $ toSeg aSig)

syncEvtTrig :: (Sigs a) => Sig -> Maybe a -> Tick -> Tick -> a -> a
syncEvtTrig bpm minitVal x st a = evtTrig minitVal (syncBpm bpm x) (syncBpm bpm st) a

-- | Toggles the signal with event stream.
evtToggle :: (Sigs a) => Maybe a -> Tick -> a -> a
evtToggle initVal evt = evtTrig initVal (fmap (const unit) ons) (fmap (const unit) offs)
  where
    (offs, ons) = splitToggle $ toTog evt

syncEvtToggle :: (Sigs a) => Sig -> Maybe a -> Tick -> a -> a
syncEvtToggle bpm initVal evt = evtToggle initVal (syncBpm bpm evt)

{- | Consider note limiting? or performance degrades
every note is held to infinity and it continues to produce zeroes.
No it's not every sequence note triggers it
but it's best to limit them anyway
-}
evtTap :: (Sigs a) => Sig -> Tick -> a -> a
evtTap dt x a = runSeg $ del x $ loop $ lim x $ toSeg $ takeSnd dt a

syncEvtTap :: (Sigs a) => Sig -> Sig -> Tick -> a -> a
syncEvtTap bpm dt x = evtTap dt (syncBpm bpm x)

{- | Plays a list signals. It triggers the signal with event stream and silences
all the rest in the list so that only one signal is playing. We can create simple
costum monosynthes with this function. The last event stream stops all signals.
-}
evtGroup :: (Sigs a) => Maybe a -> [(Tick, a)] -> Tick -> a
evtGroup initVal as stop =
  sum $
    fmap (\(a, b, c) -> evtTrig initVal a (mappend b stop) c) $
      zipWith (\n (a, sam) -> (a, mconcat $ fmap snd $ filter ((/= n) . fst) allEvts, sam)) [(0 :: Int) ..] as
  where
    allEvts :: [(Int, Tick)]
    allEvts = zip [0 ..] (fmap fst as)

syncEvtGroup :: (Sigs a) => Sig -> Maybe a -> [(Tick, a)] -> Tick -> a
syncEvtGroup bpm initVal as stop = evtGroup initVal (fmap (\(e, a) -> (syncBpm bpm e, a)) as) (syncBpm bpm stop)

-- | Triggers one signal after another with an event stream.
evtCycle :: (Sigs a) => Maybe a -> Tick -> Tick -> [a] -> a
evtCycle minitVal start stop sigs = case minitVal of
  Nothing -> ons
  Just _ -> ons + offs
  where
    ons = evtCycleNoInit start stop sigs
    offs = evtGroup minitVal [(start, 0)] stop

    evtCycleNoInit startMsg stopMsg asigs = runSeg $ loop $ lim stopMsg $ del startMsg $ loop $ mel $ fmap (lim startMsg . toSeg) asigs

-- | Triggers one signal after another with an event stream.
syncEvtCycle :: (Sigs a) => Sig -> Maybe a -> Tick -> Tick -> [a] -> a
syncEvtCycle bpm minitVal start stop sigs = evtCycle minitVal (syncBpm bpm start) (syncBpm bpm stop) sigs

-----------------------------------------------------------
-- Char sampler

{- | Triggers a signal when one of the chars from the first string is pressed.
Stops signal from playing when one of the chars from the second string is pressed.
-}
charTrig :: (Sigs a) => Maybe a -> String -> String -> a -> a
charTrig minitVal starts stops asig = case minitVal of
  Nothing -> ons
  Just initVal -> ons + offs initVal + first initVal
  where
    ons = charTrigNoInit starts stops asig
    offs initVal = charTrigNoInit stops starts initVal
    first initVal = evtTrigger loadbang (strOn starts) initVal

    charTrigNoInit startMsg stopMsg bsig = runSeg $ loop $ lim (strOn stopMsg) $ toSeg $ retrig (const $ return bsig) (strOn startMsg)

{- | Triggers a signal when one of the chars from the first string is pressed.
Stops signal from playing when one of the chars from the second string is pressed.
Synchronizes the signal with bpm (first argument).
-}
syncCharTrig :: (Sigs a) => Sig -> Maybe a -> String -> String -> a -> a
syncCharTrig bpm minitVal starts stops asig = case minitVal of
  Nothing -> ons
  Just initVal -> ons + offs initVal + first initVal
  where
    ons = charTrigNoInit starts stops asig
    offs initVal = charTrigNoInit stops starts initVal
    first initVal = syncEvtTrigger bpm loadbang (strOn starts) initVal

    charTrigNoInit startMsg stopMsg bsig = runSeg $ loop $ lim (syncBpm bpm $ strOn stopMsg) $ toSeg $ retrig (const $ return bsig) (syncBpm bpm $ strOn startMsg)

-- syncCharTrig :: (Sigs a) => Sig -> String -> String -> a -> a
-- syncCharTrig bpm starts stops asig = runSeg $ loop $ lim (syncBpm bpm $ strOn stops) $ toSeg $ retrig (const $ return asig) (syncBpm bpm $ strOn starts)

-- | Plays a signal while a key is pressed.
charPush :: (Sigs a) => Maybe a -> Char -> a -> a
charPush = genCharPush evtTrigger

-- | Plays a signal while a key is pressed. Synchronized by BPM (first argument).
syncCharPush :: (Sigs a) => Sig -> Maybe a -> Char -> a -> a
syncCharPush bpm = genCharPush (syncEvtTrigger bpm)

genCharPush :: (Sigs a) => (Tick -> Tick -> a -> a) -> Maybe a -> Char -> a -> a
genCharPush trig minitVal ch asig = case minitVal of
  Nothing -> ons
  Just v0 -> ons + offs v0 + first v0
  where
    ons = trig (charOn ch) (charOff ch) asig
    offs v = trig (charOff ch) (charOn ch) v
    first v = trig loadbang (charOn ch) v

-- | Toggles the signal when key is pressed.
charToggle :: (Sigs a) => Maybe a -> Char -> a -> a
charToggle = genCharToggle id

{- | Toggles the signal when key is pressed.
Synchronizes by BPM (first argument).
-}
syncCharToggle :: (Sigs a) => Sig -> Maybe a -> Char -> a -> a
syncCharToggle bpm = genCharToggle (syncBpm bpm)

-- | Toggles the signal when key is pressed.
genCharToggle :: (Sigs a) => (Tick -> Tick) -> Maybe a -> Char -> a -> a
genCharToggle needSync minitVal key asig =
  retrig (togInstr minitVal) $
    accumE (1 :: D) (\_ s -> (s, mod' (s + 1) 2)) $
      needSync $
        charOn key
  where
    togInstr mv0 isPlay = do
      ref <- newRef 0
      case mv0 of
        Nothing -> return ()
        Just v0 -> writeRef ref v0
      when1 (sig isPlay ==* 1) $ do
        writeRef ref asig
      readRef ref

-- Consider note limiting? or performance degrades
-- every note is held to infinity and it continues to produce zeroes.
-- No it's not every sequence note triggers it
-- but it's best to limit them anyway
charTap :: (Sigs a) => Sig -> String -> a -> a
charTap stop starts = evtTap stop (strOn starts)

syncCharTap :: (Sigs a) => Sig -> Sig -> String -> a -> a
syncCharTap bpm stop starts = syncEvtTap bpm stop (strOn starts)

{- | Plays a list of signals when corresponding key is pressed.
Turns off all other signals in the group. The last string is
for stopping the group from playing.
-}
charGroup :: (Sigs a) => Maybe a -> [(Char, a)] -> String -> a
charGroup = genCharGroup evtTrigger

{- | Plays a list of signals when corresponding key is pressed.
Turns off all other signals in the group. The last string is
for stopping the group from playing. Events are syncronized by BPM (first argument).
-}
syncCharGroup :: (Sigs a) => Sig -> Maybe a -> [(Char, a)] -> String -> a
syncCharGroup bpm = genCharGroup (syncEvtTrigger bpm)

genCharGroup :: (Sigs a) => (Tick -> Tick -> a -> a) -> Maybe a -> [(Char, a)] -> String -> a
genCharGroup trig minitVal as stop = case minitVal of
  Nothing -> charGroupNoInit trig as stop
  Just initVal -> ons + offs initVal + first initVal
  where
    ons = charGroupNoInit trig as stop
    offs initVal = charGroupNoInit trig (fmap (\ch -> (ch, initVal)) stop) onKeys
    first initVal = trig loadbang (mconcat $ fmap charOn onKeys) initVal

    onKeys = fmap fst as

charGroupNoInit :: (Sigs a) => (Tick -> Tick -> a -> a) -> [(Char, a)] -> String -> a
charGroupNoInit trig as stop = sum $ fmap f as
  where
    allKeys = fmap fst as ++ stop
    f (key, asig) = trig ons offs asig
      where
        ons = charOn key
        offs = strOn allKeys

{- | Plays signals one after another when key is pressed.
Stops the group from playing when the char from the last
argument is pressed.
-}
charCycle :: (Sigs a) => (Maybe a) -> Char -> String -> [a] -> a
charCycle initVal start stops sigs = evtCycle initVal (charOn start) (strOn stops) sigs

{- | Plays signals one after another when key is pressed.
Stops the group from playing when the char from the last
argument is pressed. Events are syncronised with BPM (first argument).
-}
syncCharCycle :: (Sigs a) => Sig -> Maybe a -> Char -> String -> [a] -> a
syncCharCycle bpm initVal start stops sigs = syncEvtCycle bpm initVal (charOn start) (strOn stops) sigs

---------------------------------------------------------------------

evtTrigger :: (Sigs a) => Tick -> Tick -> a -> a
evtTrigger ons offs asig = schedUntil (const $ return asig) ons offs

syncEvtTrigger :: (Sigs a) => Sig -> Tick -> Tick -> a -> a
syncEvtTrigger bpm ons offs asig = schedUntil (const $ return asig) (syncBpm bpm ons) (syncBpm bpm offs)

----------------------------------------------------------
-- Midi sampler

type MidiTrigFun a = a -> D -> SE a

-- | Scales the signal with the amplitude.
midiAmpInstr :: (SigSpace a, Sigs a) => a -> D -> SE a
midiAmpInstr asig amp = return $ mul (sig amp) asig

{- | Applies a low pass filter to the signal.
The first two arguments are the frequency range for center frequency of the filter
and the second one is amount of resonance (ranges from 0 to 1).
-}
midiLpInstr :: (SigSpace a, Sigs a) => (Sig, Sig) -> Sig -> a -> D -> SE a
midiLpInstr (minC, maxC) q asig amp = return $ mapSig (mlp (minC * ((maxC / minC) ** sig amp)) q) asig

-- | the midiLpInstr with audio range for center frequency.
midiAudioLpInstr :: (SigSpace a, Sigs a) => Sig -> a -> D -> SE a
midiAudioLpInstr = midiLpInstr (50, 10000)

-- | Ignores the amplitude and justplays back the original signal.
midiConstInstr :: (SigSpace a, Sigs a) => a -> D -> SE a
midiConstInstr asig _amp = return asig

{- | Plays a signal when the key is pressed. Retriggers the signal when the key is pressed again.
The key is an integer midi code. The C1 is 60 and the A1 is 69.
-}
midiTrig :: (SigSpace a, Sigs a) => MidiChn -> Int -> a -> SE a
midiTrig = midiTrigBy midiAmpInstr

{- | Plays a signal when the key is pressed. Retriggers the signal when the key is pressed again.
Turns off the signal after specified duration (n seconds).
The key is an integer midi code. The C1 is 60 and the A1 is 69.
-}
midiTap :: (SigSpace a, Sigs a) => MidiChn -> Sig -> Int -> a -> SE a
midiTap = midiTapBy midiAmpInstr

{- | Plyas a signal while the key is pressed.
The key is an integer midi code. The C1 is 60 and the A1 is 69.
-}
midiPush :: (SigSpace a, Sigs a) => MidiChn -> Int -> a -> SE a
midiPush = midiPushBy midiAmpInstr

{- | Plays and stops a signal in the toggle mode.
The key is an integer midi code. The C1 is 60 and the A1 is 69.
-}
midiToggle :: (SigSpace a, Sigs a) => MidiChn -> Int -> a -> SE a
midiToggle = midiToggleBy midiAmpInstr

{- | Plays a set of signals on the list of keys. When certain
key is pressed the corresponding signal starts to play and all
the rest are stopped.

-- The key is an integer midi code. The C1 is 60 and the A1 is 69.
-}
midiGroup :: (SigSpace a, Sigs a) => MidiChn -> [(Int, a)] -> SE a
midiGroup = midiGroupBy midiAmpInstr

{- | The generic midiTrig. We can specify the midi function.
The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
It produces some output. The default is scaling the signal with the amplitude.
-}
midiTrigBy :: (SigSpace a, Sigs a) => MidiTrigFun a -> MidiChn -> Int -> a -> SE a
midiTrigBy midiInstr midiChn key asig = fmap (\evt -> retrig (midiInstr asig) evt) (midiKeyOn midiChn $ int key)

{- | The generic midiTap. We can specify the midi function.
The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
It produces some output. The default is scaling the signal with the amplitude.
-}
midiTapBy :: (SigSpace a, Sigs a) => MidiTrigFun a -> MidiChn -> Sig -> Int -> a -> SE a
midiTapBy midiInstr midiChn dt key asig = midiTrigBy midiInstr midiChn key (takeSnd dt asig)

{- | The generic midiPush. We can specify the midi function.
The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
It produces some output. The default is scaling the signal with the amplitude.
-}
midiPushBy :: (SigSpace a, Sigs a) => MidiTrigFun a -> MidiChn -> Int -> a -> SE a
midiPushBy midiInstr midiChn key asig = do
  ons <- midiKeyOn midiChn (int key)
  offs <- midiKeyOff midiChn (int key)
  return $ midiEvtTriggerBy midiInstr ons offs asig

{- | The generic midiToggle. We can specify the midi function.
The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
It produces some output. The default is scaling the signal with the amplitude.
-}
midiToggleBy :: (SigSpace a, Sigs a) => MidiTrigFun a -> MidiChn -> Int -> a -> SE a
midiToggleBy midiInstr midiChn key asig =
  fmap
    (\evt -> retrig togMidiInstr evt)
    (fmap (accumE (1 :: D) (\a s -> ((a, s), mod' (s + 1) 2))) $ midiKeyOn midiChn $ int key)
  where
    togMidiInstr (amp, isPlay) = do
      ref <- newRef 0
      when1 (sig isPlay ==* 1) $ do
        writeRef ref =<< midiInstr asig amp
      readRef ref

{- | The generic midiGroup. We can specify the midi function.
The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
It produces some output. The default is scaling the signal with the amplitude.
-}
midiGroupBy :: (SigSpace a, Sigs a) => MidiTrigFun a -> MidiChn -> [(Int, a)] -> SE a
midiGroupBy midiInstr midiChn as = fmap sum $ mapM f as
  where
    allKeys = fmap fst as
    f (key, asig) = do
      ons <- midiKeyOn midiChn (int key)
      offs <- fmap (fmap (const unit) . mconcat) $ mapM (midiKeyOn midiChn . int) allKeys
      return $ midiEvtTriggerBy midiInstr ons offs asig

midiEvtTriggerBy :: (SigSpace a, Sigs a) => (a -> D -> SE a) -> Evt D -> Tick -> a -> a
midiEvtTriggerBy midiInstr ons offs asig = schedUntil (midiInstr asig) ons offs

-----------------------------------------------------------
-- misc

keyColumn1, keyColumn2, keyColumn3, keyColumn4, keyColumn5, keyColumn6, keyColumn7, keyColumn8, keyColumn9, keyColumn0 :: [Char]
keyColumn1 = ['1', 'q', 'a', 'z']
keyColumn2 = ['2', 'w', 's', 'x']
keyColumn3 = ['3', 'e', 'd', 'c']
keyColumn4 = ['4', 'r', 'f', 'v']
keyColumn5 = ['5', 't', 'g', 'b']
keyColumn6 = ['6', 'y', 'h', 'n']
keyColumn7 = ['7', 'u', 'j', 'm']
keyColumn8 = ['8', 'i', 'k', ',']
keyColumn9 = ['9', 'o', 'l', '.']
keyColumn0 = ['0', 'p', ';', '/']

keyColumns :: [[Char]]
keyColumns = [keyColumn1, keyColumn2, keyColumn3, keyColumn4, keyColumn5, keyColumn6, keyColumn7, keyColumn8, keyColumn9, keyColumn0]
