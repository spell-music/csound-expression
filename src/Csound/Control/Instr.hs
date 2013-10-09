{-# Language TypeFamilies, FlexibleContexts, FlexibleInstances #-}
module Csound.Control.Instr(
    Fun(..),     
    
    -- * Mix
    CsdSco(..), Mix, sco, mix, eff, CsdEventList(..), CsdEvent, 

    -- * Midi
    Msg, Channel, MidiInstr(..), midi, midin, pgmidi,
    -- ** Reading midi note parameters
    cpsmidi, ampmidi,

    -- * Evt            
    trig, sched, schedHarp, autoOff,
    
    -- * Specific instruments
    CpsInstr(..), AmpInstr(..)
) where

import Csound.Typed hiding (midi, pgmidi)
import qualified Csound.Typed as T(midi, pgmidi)
import Csound.Typed.Opcode

class Fun a where
    type FunOut a :: *
    type FunIn  a :: *

instance Fun (a -> b) where
    type FunOut (a -> b) = b
    type FunIn  (a -> b) = a

class (Fun f, Out (FunOut f)) => CpsInstr f where
    cpsInstr :: f -> ((D, D) -> FunOut f)

instance Out a => CpsInstr (Sig -> a) where
    cpsInstr f (amp, cps) = mapOut (sig amp * ) $ f (sig cps) 

instance Out a => CpsInstr (D -> a) where
    cpsInstr f (amp, cps) = mapOut (sig amp * ) $ f cps

class (Fun f, Out (FunOut f)) => AmpInstr f where
    ampInstr :: f -> (D -> FunOut f)

instance Out a => AmpInstr (Sig -> a) where
    ampInstr boom amp = boom (sig amp)

instance Out a => AmpInstr (D -> a) where
    ampInstr = id

instance Fun Sig where
    type FunOut Sig = Sig
    type FunIn  Sig = ()

instance Fun (SE Sig) where
    type FunOut (SE Sig) = (SE Sig)
    type FunIn  (SE Sig) = ()

instance Fun (Sig, Sig) where
    type FunOut (Sig, Sig) = (Sig, Sig)
    type FunIn  (Sig, Sig) = ()

instance Fun (SE (Sig, Sig)) where
    type FunOut (SE (Sig, Sig)) = SE (Sig, Sig)
    type FunIn  (SE (Sig, Sig)) = ()

instance AmpInstr Sig where
    ampInstr boom amp = sig amp * boom

instance AmpInstr (SE Sig) where
    ampInstr boom amp = mapOut (sig amp * ) boom

instance AmpInstr (Sig, Sig) where
    ampInstr boom amp = mapOut (sig amp * ) boom

instance AmpInstr (SE (Sig, Sig)) where
    ampInstr boom amp = mapOut (sig amp * ) boom

--------------------------------------------------------------------------
-- midi

midi :: MidiInstr a => a -> NoSE (FunOut a)
midi = midin 0

midin :: MidiInstr a => Channel -> a -> NoSE (FunOut a)
midin chn instr = T.midi chn (fromMidiInstr instr)

pgmidi :: MidiInstr a => Maybe Int -> Channel -> a -> NoSE (FunOut a)
pgmidi mn chn instr = T.pgmidi mn chn (fromMidiInstr instr)

ampCps :: Msg -> (D, D)
ampCps msg = (ampmidi msg, cpsmidi msg)

-- midi instruments

class (Fun a, Out (NoSE (FunOut a)), Out (FunOut a)) => MidiInstr a where
    fromMidiInstr :: a -> (Msg -> FunOut a)

-- identity

instance (Out (NoSE a), Out a) => MidiInstr (Msg -> a) where
    fromMidiInstr = id

-- amplitude and frequency

instance (Out (NoSE a), Out a) => MidiInstr ((D, D) -> a) where
    fromMidiInstr f = f . ampCps

instance (Out (NoSE a), Out a) => MidiInstr ((D, Sig) -> a) where
    fromMidiInstr f = f . (\(amp, cps) -> (amp, sig cps)) . ampCps

instance (Out (NoSE a), Out a) => MidiInstr ((Sig, D) -> a) where
    fromMidiInstr f = f . (\(amp, cps) -> (sig amp, cps)) . ampCps

instance (Out (NoSE a), Out a) => MidiInstr ((Sig, Sig) -> a) where
    fromMidiInstr f = f . (\(amp, cps) -> (sig amp, sig cps)) . ampCps

-- only frequency

instance (Out (NoSE a), Out a) => MidiInstr (D -> a) where
    fromMidiInstr f msg = mapOut ((sig amp) * ) $ f cps
        where (amp, cps) = ampCps msg

instance (Out (NoSE a), Out a) => MidiInstr (Sig -> a) where
    fromMidiInstr f msg = mapOut ((sig amp) * ) $ f (sig cps)
        where (amp, cps) = ampCps msg

