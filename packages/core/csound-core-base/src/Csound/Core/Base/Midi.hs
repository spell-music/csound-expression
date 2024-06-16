module Csound.Core.Base.Midi
  ( Msg
  , MidiChannel
  , midi
  , midin
  , pgmidi
  , midi_
  , midin_
  , pgmidi_
  , initMidiCtrl
  , MidiFun
  , ampCps
  , onMsg
  ) where

import Csound.Core.Base.Midi.Internal
import Csound.Core.Types
import Csound.Core.Opcode
import Csound.Core.Base.Evt
import Data.Boolean
import Csound.Core.Base.Instr (alwaysOn)

type MidiFun a = (Msg -> SE a) -> SE a

-- | Specifies the midi channel or programm.
data MidiChn = ChnAll | Chn Int | Pgm (Maybe Int) Int
  deriving (Show, Eq)

ampCps :: Msg -> (D, D)
ampCps _msg = (ampmidi 1, cpsmidi)

onMsg :: ((D, D) -> SE Sig2) -> Msg -> SE Sig2
onMsg instr = instr . ampCps

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

{-
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
trigNamedMono :: Str -> SE MonoArg
trigNamedMono name = namedMonoMsg name

namedAmpCpsSig:: Str -> SE (Sig, Sig, Sig)
namedAmpCpsSig name = do
  ref <- newCtrlRef ((0, 0) :: (Sig, Sig))
  statusRef <- newCtrlRef (0 :: Sig)
  status <- trigByNameMidi name (instr statusRef ref)
  writeRef statusRef status
  let resStatus = ifB (downsamp status ==* 0) 0 1
  (amp, cps) <- readRef ref
  return (downsamp amp, downsamp cps, resStatus)
  where
    instr :: Ref Sig -> Ref (Sig, Sig) -> (D, D, ()) -> SE Sig
    instr statusRef hNote (pitchKey, volKey, _) = do
      curId <- readRef statusRef
      myIdRef <- newLocalRef (toD $ ir curId)
      myId <- readRef myIdRef
      when1 (curId ==* (toSig $ myId + 1)) $ do
        writeRef hNote (toSig volKey, toSig pitchKey)
      return 1

-- | Creates an instrument that can be triggered by name with Csound API.
--
-- It's intended to be used like a midi instrument. It simulates a simplified midi protocol.
-- We can trigger notes:
--
-- > i "givenName" delay duration 1 pitchKey volumeKey auxParams     -- note on
-- > i "givenName" delay duration 0 pitchKey volumeKey auxParams     -- note off
--
-- The arguments are
--
-- > trigByNameMidi name instrument
--
-- The instrument takes a triplet of @(pitchKey, volumeKey, auxilliaryTuple)@.
-- The order does matter. Please don't pass the @volumeKey@ as the first argument.
-- The instrument expects the pitch key to be a first argument.

-- Under the hood
-- it creates held notes that are indexed by pitch. If you know the Csound it creates
-- the notes with indexes:
--
-- > i 18.pitchKey
--
-- Here the 18 is some generated integer index. And then on receiving a note a note off message for the specific key the
-- Csound procedure invokes:
--
-- > turnoff 18.pitchKey
trigByNameMidi  :: (Arg a, Sigs b) => Str -> ((D, D, a) -> SE b) -> SE b
trigByNameMidi name instr = do
    ref <- newClearableRef 0
    trigByNameMidi_ name (go ref)
    readRef ref
    where go ref x = mixRef ref =<< instr x

-- | It behaves just like the function @trigByNameMidi@. Only it doesn't produce an audio
-- signal. It performs some procedure on note on and stops doing the precedure on note off.
trigByNameMidi_ :: forall a . Arg a => Str -> ((D, D, a) -> SE ()) -> SE ()
trigByNameMidi_ name instr = do
  instrId <- newNamedProc name instr
  trigByName_ name (go instrId)
  where
    go :: D.InstrId -> (D, D, D, a) -> SE ()
    go instrId (noteFlag, pch, vol, other) = fromDep_ $ hideGEinDep $ do
        pchExpr      <- toGE pch
        let instrIdExpr = D.instrIdE instrId + pchExpr / 1000
        noteFlagExpr <- toGE noteFlag
        args <- fromTuple (pch, vol, other)
        return $ do
                D.when1 D.IfIr (noteFlagExpr ==* 1) $ do
                    eventi (Event instrIdExpr 0 (-1) args)
                D.when1 D.IfIr (noteFlagExpr ==* 0) $ do
                    eventi (Event (negate instrIdExpr) 0 0 args)
                turnoff
-}

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
  chRef  <- newCtrlRef (0 :: Sig)
  evtRef <- newCtrlRef (0 :: Sig)
  writeRef chRef =<< midiFun instr

  alwaysOn $ do
    a <- readRef chRef
    writeRef evtRef $ diff a

  evtSig <- readRef evtRef
  return $ filterE (( >* 0) . toSig) $ snaps evtSig
  where
    instr msg = do
      note <- notnum
      print' [note]
      return $ ifB (boolSig $ note ==* key) (toSig $ ampmidi 1) 0

midiKeyOffBy :: MidiFun Sig -> D -> SE Tick
midiKeyOffBy midiFun key = do
  chRef  <- newCtrlRef (0 :: Sig)
  evtRef <- newCtrlRef (0 :: Sig)
  writeRef chRef =<< midiFun instr

  alwaysOn $ do
    a <- readRef chRef
    writeRef evtRef $ diff a

  evtSig <- readRef evtRef
  return $ fmap (const ()) $ filterE (( `less` 0) . toSig) $ snaps evtSig
  where
    instr msg = do
      note <- notnum
      print' [note]
      return $ ifB (boolSig $ note ==* key) (toSig $ ampmidi 1) 0
