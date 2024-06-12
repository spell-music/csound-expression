module Csound.Typed.Opcode.PitchConverters (
    
    
    -- * Functions.
    cent, cpsmidinn, cpsoct, cpspch, ftom, mtof, mton, ntom, octave, octcps, octmidinn, octpch, pchmidinn, pchoct, pchtom, semitone,
    
    -- * Tuning Opcodes.
    cps2pch, cpstun, cpstuni, cpsxpch) where

import Csound.Dynamic
import Csound.Typed

-- Functions.

-- | 
-- Calculates a factor to raise/lower a frequency by a given amount of cents.
--
-- >  cent (x) 
--
-- csound doc: <http://csound.com/docs/manual/cent.html>
cent :: SigOrD a => a -> a
cent b1 =
  fromGE $ f <$> toGE b1
  where
    f a1 = opr1 "cent" a1

-- | 
-- Converts a Midi note number value to cycles-per-second.
--
-- >  cpsmidinn  (MidiNoteNumber)  (init- or control-rate args only)
--
-- csound doc: <http://csound.com/docs/manual/cpsmidinn.html>
cpsmidinn :: SigOrD a => a -> a
cpsmidinn b1 =
  fromGE $ f <$> toGE b1
  where
    f a1 = opr1k "cpsmidinn" a1

-- | 
-- Converts an octave-point-decimal value to cycles-per-second.
--
-- >  cpsoct  (oct)  (no rate restriction)
--
-- csound doc: <http://csound.com/docs/manual/cpsoct.html>
cpsoct :: SigOrD a => a -> a
cpsoct b1 =
  fromGE $ f <$> toGE b1
  where
    f a1 = opr1 "cpsoct" a1

-- | 
-- Converts a pitch-class value to cycles-per-second.
--
-- >  cpspch  (pch)  (init- or control-rate args only)
--
-- csound doc: <http://csound.com/docs/manual/cpspch.html>
cpspch :: SigOrD a => a -> a
cpspch b1 =
  fromGE $ f <$> toGE b1
  where
    f a1 = opr1k "cpspch" a1

-- | 
-- Convert frequency to midi
--
-- Convert frequency to midi note number, taking global value
-- 	  of A4 into account.
--
-- > imidi  ftom  ifreq
-- > kmidi  ftom  kfreq
--
-- csound doc: <http://csound.com/docs/manual/ftom.html>
ftom ::  D -> Sig
ftom b1 =
  Sig $ f <$> unD b1
  where
    f a1 = opcs "ftom" [(Ir,[Ir]),(Kr,[Kr])] [a1]

-- | 
-- Convert a midi to frequency
--
-- Convert a midi note number value to cycles per second, taking
-- 	  global value of A4 into account.
--
-- > ifreq  mtof  imidi
-- > kfreq  mtof  kmidi
--
-- csound doc: <http://csound.com/docs/manual/mtof.html>
mtof ::  D -> Sig
mtof b1 =
  Sig $ f <$> unD b1
  where
    f a1 = opcs "mtof" [(Ir,[Ir]),(Kr,[Kr])] [a1]

-- | 
-- Convert midi note number to string note name
--
-- Convert midi note number to string note name, with an accuracy
-- 	  of 1 cent.
--
-- > Snote  mton  kmidi
-- > Snote  mton  imidi
--
-- csound doc: <http://csound.com/docs/manual/mton.html>
mton ::  Sig -> Str
mton b1 =
  Str $ f <$> unSig b1
  where
    f a1 = opcs "mton" [(Sr,[Kr]),(Sr,[Ir])] [a1]

-- | 
-- Convert note name to midi note number
--
-- Convert note name to midi note number. It allows note name to
-- 	  include microtones or a deviation in cents.
--
-- > kmidi  ntom  Snote
-- > imidi  ntom  Snote
--
-- csound doc: <http://csound.com/docs/manual/ntom.html>
ntom ::  Str -> D
ntom b1 =
  D $ f <$> unStr b1
  where
    f a1 = opcs "ntom" [(Kr,[Sr]),(Ir,[Sr])] [a1]

-- | 
-- Calculates a factor to raise/lower a frequency by a given amount of octaves.
--
-- >  octave (x)
--
-- csound doc: <http://csound.com/docs/manual/octave.html>
octave :: SigOrD a => a -> a
octave b1 =
  fromGE $ f <$> toGE b1
  where
    f a1 = opr1 "octave" a1

-- | 
-- Converts a cycles-per-second value to octave-point-decimal.
--
-- >  octcps  (cps)  (init- or control-rate args only)
--
-- csound doc: <http://csound.com/docs/manual/octcps.html>
octcps :: SigOrD a => a -> a
octcps b1 =
  fromGE $ f <$> toGE b1
  where
    f a1 = opr1k "octcps" a1

-- | 
-- Converts a Midi note number value to octave-point-decimal.
--
-- >  octmidinn  (MidiNoteNumber)  (init- or control-rate args only)
--
-- csound doc: <http://csound.com/docs/manual/octmidinn.html>
octmidinn :: SigOrD a => a -> a
octmidinn b1 =
  fromGE $ f <$> toGE b1
  where
    f a1 = opr1k "octmidinn" a1

-- | 
-- Converts a pitch-class value to octave-point-decimal.
--
-- >  octpch  (pch)  (init- or control-rate args only)
--
-- csound doc: <http://csound.com/docs/manual/octpch.html>
octpch :: SigOrD a => a -> a
octpch b1 =
  fromGE $ f <$> toGE b1
  where
    f a1 = opr1k "octpch" a1

-- | 
-- Converts a Midi note number value to octave point pitch-class units.
--
-- >  pchmidinn  (MidiNoteNumber)  (init- or control-rate args only)
--
-- csound doc: <http://csound.com/docs/manual/pchmidinn.html>
pchmidinn :: SigOrD a => a -> a
pchmidinn b1 =
  fromGE $ f <$> toGE b1
  where
    f a1 = opr1k "pchmidinn" a1

-- | 
-- Converts an octave-point-decimal value to pitch-class.
--
-- >  pchoct  (oct)  (init- or control-rate args only)
--
-- csound doc: <http://csound.com/docs/manual/pchoct.html>
pchoct :: SigOrD a => a -> a
pchoct b1 =
  fromGE $ f <$> toGE b1
  where
    f a1 = opr1k "pchoct" a1

-- | 
-- Convert pch to midi note number
--
-- Convert pch to midi note number. pch representation has the form
-- 	  Octave.pitchclass,
-- 	  pitchclass being a number between 00 and 12.
--
-- > imidi  pchtom  ipch
-- > kmidi  pchtom  kpch
--
-- csound doc: <http://csound.com/docs/manual/pchtom.html>
pchtom ::  D -> Sig
pchtom b1 =
  Sig $ f <$> unD b1
  where
    f a1 = opcs "pchtom" [(Ir,[Ir]),(Kr,[Kr])] [a1]

-- | 
-- Calculates a factor to raise/lower a frequency by a given amount of semitones.
--
-- >  semitone (x)
--
-- csound doc: <http://csound.com/docs/manual/semitone.html>
semitone :: SigOrD a => a -> a
semitone b1 =
  fromGE $ f <$> toGE b1
  where
    f a1 = opr1 "semitone" a1

-- Tuning Opcodes.

-- | 
-- Converts a pitch-class value into cycles-per-second (Hz) for equal divisions of the octave.
--
-- > icps  cps2pch  ipch, iequal
--
-- csound doc: <http://csound.com/docs/manual/cps2pch.html>
cps2pch ::  D -> D -> D
cps2pch b1 b2 =
  D $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "cps2pch" [(Ir,[Ir,Ir])] [a1,a2]

-- | 
-- Returns micro-tuning values at k-rate.
--
-- > kcps  cpstun  ktrig, kindex, kfn
--
-- csound doc: <http://csound.com/docs/manual/cpstun.html>
cpstun ::  Sig -> Sig -> Tab -> Sig
cpstun b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unTab b3
  where
    f a1 a2 a3 = opcs "cpstun" [(Kr,[Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Returns micro-tuning values at init-rate.
--
-- > icps  cpstuni  index, ifn
--
-- csound doc: <http://csound.com/docs/manual/cpstuni.html>
cpstuni ::  D -> Tab -> D
cpstuni b1 b2 =
  D $ f <$> unD b1 <*> unTab b2
  where
    f a1 a2 = opcs "cpstuni" [(Ir,[Ir,Ir])] [a1,a2]

-- | 
-- Converts a pitch-class value into cycles-per-second (Hz) for equal divisions of any interval.
--
-- Converts a pitch-class value into cycles-per-second (Hz) for equal divisions of any interval. There is a restriction of no more than 100 equal divisions.
--
-- > icps  cpsxpch  ipch, iequal, irepeat, ibase
--
-- csound doc: <http://csound.com/docs/manual/cpsxpch.html>
cpsxpch ::  D -> D -> D -> D -> D
cpsxpch b1 b2 b3 b4 =
  D $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "cpsxpch" [(Ir,[Ir,Ir,Ir,Ir])] [a1,a2,a3,a4]