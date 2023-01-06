{-# Language FlexibleContexts #-}
-- | Midi.
module Csound.Control.Midi(
    MidiChn(..), MidiFun, toMidiFun, toMidiFun_,
    Msg, Channel, midi, midin, pgmidi, ampCps,
    midi_, midin_, pgmidi_,
    -- * Mono-midi synth
    monoMsg, holdMsg, trigNamedMono, genMonoMsg, smoothMonoArg,
    genFilteredMonoMsg, genFilteredMonoMsgTemp,

    -- ** Custom temperament
    monoMsgTemp, holdMsgTemp, genMonoMsgTemp,
    -- * Midi event streams
    midiKeyOn, midiKeyOff,
    -- * Reading midi note parameters
    cpsmidi, ampmidi, initc7, ctrl7, midiCtrl7, midiCtrl, umidiCtrl,
    midiCtrl7A, midiCtrlA, umidiCtrlA,
    ampmidinn,

    -- ** Custom temperament
    ampCps', cpsmidi', cpsmidi'D, cpsmidi'Sig,

    -- * Overload
    tryMidi, tryMidi', MidiInstr(..), MidiInstrTemp(..),

    -- * Other
    namedAmpCpsSig
) where

import Data.Boolean
import Data.Text (Text)

import Csound.Typed hiding (arg)
import Csound.Typed.Opcode hiding (initc7)
import Csound.Control.Overload
import Csound.Control.Instr(alwaysOn)
import Csound.Control.Evt(Tick)
import Csound.Types

import Csound.Tuning

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

-- | Midi message convertion with custom temperament.
ampCps' :: Temp -> Msg -> (D, D)
ampCps' temp msg = (ampmidi msg 1, cpsmidi' temp msg)

-- | Midi message convertion to Hz with custom temperament.
cpsmidi' :: Temp -> Msg -> D
cpsmidi' (Temp t) msg = cpstmid msg t

-- | Midi pitch key convertion to Hz with custom temperament. It works on constants.
cpsmidi'D :: Temp -> D -> D
cpsmidi'D (Temp t) key = cpstuni key t

-- | Midi pitch key convertion to Hz with custom temperament. It works on signals.
cpsmidi'Sig :: Temp -> Sig -> Sig
cpsmidi'Sig (Temp t) key = cpstun 1 key t

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
monoMsg = smoothMonoMsg cpsmidi

-- | Produces midi amplitude and frequency as a signal.
-- The signal fades out when nothing is pressed.
-- It can be used in mono-synths. Arguments are custom temperament, midi channel, portamento time
-- and release time. A portamento time is time it takes for transition
-- from one note to another.
--
-- > monoMsgTemp temperament channel portamentoTime releaseTime
monoMsgTemp :: Temp -> MidiChn -> D -> D -> SE (Sig, Sig)
monoMsgTemp tm = smoothMonoMsg (cpsmidi' tm)

-- | Produces an argument for monophonic midi-synth.
-- The signal fades out when nothing is pressed.
-- It can be used in mono-synths.
--
-- > genMonoMsg channel
genMonoMsg :: MidiChn -> SE MonoArg
genMonoMsg chn = genAmpCpsSig cpsmidi (toMidiFun chn)

-- | Just like mono @genMonoMsg@ but also we can alter the temperament. The temperament spec goes first.
--
-- > genMonoMsgTemp temperament channel
genMonoMsgTemp :: Temp -> MidiChn -> SE MonoArg
genMonoMsgTemp tm chn = genAmpCpsSig (cpsmidi' tm) (toMidiFun chn)

smoothMonoArg :: D -> MonoArg -> MonoArg
smoothMonoArg time arg = arg { monoAmp = port (monoAmp arg) time, monoCps = port (monoCps arg) time }

smoothMonoMsg :: (Msg -> D) -> MidiChn -> D -> D -> SE (Sig, Sig)
smoothMonoMsg key2cps chn portTime relTime = do
  (MonoArg amp cps status _) <- genAmpCpsSig key2cps (toMidiFun chn)
  return (port amp portTime * port status relTime,  port cps portTime)


genFilteredMonoMsg :: MidiChn -> (D -> BoolD) -> SE MonoArg
genFilteredMonoMsg chn condition = filteredGenAmpCpsSig cpsmidi (toMidiFun chn) condition

-- | Just like mono @genMonoMsg@ but also we can alter the temperament. The temperament spec goes first.
--
-- > genMonoMsgTemp temperament channel
genFilteredMonoMsgTemp :: Temp -> MidiChn -> (D -> BoolD) -> SE MonoArg
genFilteredMonoMsgTemp tm chn condition = filteredGenAmpCpsSig (cpsmidi' tm) (toMidiFun chn) condition

-- | Produces midi amplitude and frequency as a signal and holds the
-- last value till the next one is present.
-- It can be used in mono-synths. Arguments are portamento time
-- and release time. A portamento time is time it takes for transition
-- from one note to another.
--
-- > holdMsg portamentoTime
holdMsg :: MidiChn -> D -> SE (Sig, Sig)
holdMsg = genHoldMsg cpsmidi

-- | Produces midi amplitude and frequency as a signal and holds the
-- last value till the next one is present.
-- It can be used in mono-synths. Arguments are portamento time
-- and release time. A portamento time is time it takes for transition
-- from one note to another.
--
-- > holdMsg portamentoTime
holdMsgTemp :: Temp -> MidiChn -> D -> SE (Sig, Sig)
holdMsgTemp tm = genHoldMsg (cpsmidi' tm)

genHoldMsg :: (Msg -> D) -> MidiChn -> D -> SE (Sig, Sig)
genHoldMsg key2cps channel portTime = do
  (amp, cps) <- genHoldAmpCpsSig key2cps (toMidiFun_ channel)
  return (port amp portTime,  port cps portTime)



genAmpCpsSig :: (Msg -> D) -> ((Msg -> SE Sig) -> SE Sig) -> SE MonoArg
genAmpCpsSig key2cps midiFun = do
    ref <- newGlobalCtrlRef ((0, 0) :: (Sig, Sig))
    status <- midiFun (instr ref)
    (amp, cps) <- readRef ref
    return $ makeMonoArg (amp, cps) status
  where
        makeMonoArg (amp, cps) status = MonoArg kamp kcps resStatus retrig
            where
                kamp = downsamp amp
                kcps = downsamp cps
                kstatus = downsamp status
                resStatus = ifB (kstatus ==* 0) 0 1
                retrig = changed [kamp, kcps, kstatus]

        instr :: Ref (Sig, Sig) -> Msg -> SE Sig
        instr hNote msg = do
            writeRef hNote (sig $ ampmidi msg 1, sig $ key2cps msg)
            return 1

filteredGenAmpCpsSig :: (Msg -> D) -> ((Msg -> SE Sig) -> SE Sig) -> (D -> BoolD) -> SE MonoArg
filteredGenAmpCpsSig key2cps midiFun condition  = do
    ref <- newGlobalCtrlRef ((0, 0) :: (Sig, Sig))
    status <- midiFun (instr ref)
    (amp, cps) <- readRef ref
    return $ makeMonoArg (amp, cps) status
    where
        makeMonoArg (amp, cps) status = MonoArg kamp kcps resStatus retrig
            where
                kamp = downsamp amp
                kcps = downsamp cps
                kstatus = downsamp status
                resStatus = ifB (kstatus ==* 0) 0 1
                retrig = changed [kamp, kcps, kstatus]

        instr :: Ref (Sig, Sig) -> Msg -> SE Sig
        instr hNote msg = do
            resRef <- newRef 0
            whenElseD (condition $ key2cps msg)
                (do
                    writeRef hNote (sig $ ampmidi msg 1, sig $ key2cps msg)
                    writeRef resRef 1)
                (do
                    writeRef resRef 0)
            readRef resRef

genHoldAmpCpsSig :: (Msg -> D) -> ((Msg -> SE ()) -> SE ()) -> SE (Sig, Sig)
genHoldAmpCpsSig key2cps midiFun = do
  ref <- newGlobalCtrlRef ((0, 0) :: (Sig, Sig))
  midiFun (instr ref)
  (amp, cps) <- readRef ref
  return (downsamp amp, downsamp cps)
  where
    instr :: Ref (Sig, Sig) -> Msg -> SE ()
    instr hNote msg = do
      writeRef hNote (sig $ ampmidi msg 1, sig $ key2cps msg)

-- | Creates a named instrument that can be triggered with Csound API.
-- This way we can create a csd file that can be used inside another program/language.
--
-- It simulates the input for monophonic midi-like instrument. Notes are encoded with messages:
--
-- > i "givenName" 1 pitchKey volumeKey     -- note on
-- > i "givenName" 0 pitchKey volumeKey     -- note off
--
-- The output is a pair of signals @(midiVolume, midiPitch)@.
trigNamedMono :: Text -> SE MonoArg
trigNamedMono name = namedMonoMsg name

namedAmpCpsSig:: Text -> SE (Sig, Sig, Sig)
namedAmpCpsSig name = do
  ref <- newGlobalCtrlRef ((0, 0) :: (Sig, Sig))
  statusRef <- newGlobalCtrlRef (0 :: Sig)
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
  chRef  <- newGlobalCtrlRef (0 :: Sig)
  evtRef <- newGlobalCtrlRef (0 :: Sig)
  writeRef chRef =<< midiFun instr

  alwaysOn $ do
    a <- readRef chRef
    writeRef evtRef $ diff a

  evtSig <- readRef evtRef
  return $ filterE (( >* 0) . sig) $ snaps evtSig
  where
    instr msg = do
      print' [notnum msg]
      return $ ifB (boolSig $ notnum msg ==* key) (sig $ ampmidi msg 1) 0


midiKeyOffBy :: MidiFun Sig -> D -> SE Tick
midiKeyOffBy midiFun key = do
  chRef  <- newGlobalCtrlRef (0 :: Sig)
  evtRef <- newGlobalCtrlRef (0 :: Sig)
  writeRef chRef =<< midiFun instr

  alwaysOn $ do
    a <- readRef chRef
    writeRef evtRef $ diff a

  evtSig <- readRef evtRef
  return $ fmap (const unit) $ filterE (( `lessThan` 0) . sig) $ snaps evtSig
  where
    instr msg = do
      print' [notnum msg]
      return $ ifB (boolSig $ notnum msg ==* key) (sig $ ampmidi msg 1) 0

--------------------------------------------------------------

-- | Initialization of the midi control-messages.
initc7 :: D -> D -> D -> SE ()
initc7 = initMidiCtrl

-- | Initializes control rate midi control and get the value in the specified range.
midiCtrl7 :: D -> D -> D -> D -> D -> SE Sig
midiCtrl7 chno ctrlno ival imin imax = do
    initc7 chno ctrlno ival
    return $ kr $ ctrl7 chno ctrlno imin imax

-- | Initializes control rate midi control and get the value in the range (-1) to 1.
midiCtrl :: D -> D -> D -> SE Sig
midiCtrl chno ctrlno ival = midiCtrl7 chno ctrlno ival (-1) 1

-- | Unipolar control rate midiCtrl. Initializes midi control and get the value in the range 0 to 1.
umidiCtrl :: D -> D -> D -> SE Sig
umidiCtrl chno ctrlno ival = midiCtrl7 chno ctrlno ival 0 1

-- | Initializes audio-rate midi control and get the value in the specified range.
midiCtrl7A :: D -> D -> D -> D -> D -> SE Sig
midiCtrl7A chno ctrlno ival imin imax = do
    initc7 chno ctrlno ival
    return $ ar $ ctrl7 chno ctrlno imin imax

-- | Initializes audio-rate midi control and get the value in the range (-1) to 1.
midiCtrlA :: D -> D -> D -> SE Sig
midiCtrlA chno ctrlno ival = midiCtrl7A chno ctrlno ival (-1) 1

-- | Unipolar audio-rate midiCtrl. Initializes midi control and get the value in the range 0 to 1.
umidiCtrlA :: D -> D -> D -> SE Sig
umidiCtrlA chno ctrlno ival = midiCtrl7A chno ctrlno ival 0 1

--------------------------------------------------------------

-- | Invokes ooverloaded instruments with midi.
-- Example:
--
-- > dac $ tryMidi (mul (fades 0.01 0.1) . tri)
tryMidi :: (MidiInstr a, Sigs (MidiInstrOut a)) => a -> SE (MidiInstrOut a)
tryMidi x = midi $ onMsg x

-- | Invokes ooverloaded instruments with midi and custom temperament.
-- Example:
--
-- > dac $ tryMidi' youngTemp2 (mul (fades 0.01 0.1) . tri)
tryMidi' :: (MidiInstrTemp a, Sigs (MidiInstrOut a)) => Temp -> a -> SE (MidiInstrOut a)
tryMidi' tm x = midi $ onMsg' tm x
