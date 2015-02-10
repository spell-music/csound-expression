-- | Sound fonts. Playing Sf2 samples. 
--
-- There are three groups of functions.
-- Functions that are defined for midi messages, midi notes (it's a pair of integers from 0-127) 
-- and  the frequencies (in Hz).
-- Each group contains four functions. They are destinguished by suffixes.
-- The function with no suffix play a sf2 file with linear interpolation 
-- and take stereo output.
-- The function with suffix @3@ read samples with cubic interpolation. 
-- The functions with suffix @m@ produce mono outputs.
-- The loopers play samples in loops.
module Csound.Control.Sf(
    Sf(Sf), sf2, 
    -- * Midi message
    sfMsg, sfMsg3, sfMsgm, sfMsg3m, sfMsgLooper,
    -- * Midi note
    sfKey, sfKey3, sfKeym, sfKey3m, sfKeyLooper,
    -- * Frequency in Hz
    sfCps, sfCps3, sfCpsm, sfCps3m, sfCpsLooper
) where

import Csound.Typed
import Csound.Typed.Opcode
import Csound.SigSpace

-- | Creates a midi instrument from sf2 sound font.
-- Midi listens on all channels. It's useful to quickly
-- test a sound font. The second argument is a sustain in seconds.
-- How long it takes for the sound to decay.
sf2 :: Sf -> D -> SE (Sig, Sig)
sf2 sf sust = midi $ sfMsg3 sf sust

-----------------------------------

-- | Creates a midi instrument from sf2 sound font file.
-- The second argument is sustain in seconds.
-- Reads samples with linear interpolation.
sfMsg :: Sf -> D -> Msg -> SE (Sig, Sig)
sfMsg = genSfMsg sfplay

-- | Creates a midi instrument from sf2 sound font file.
-- The second argument is sustain in seconds.
-- Reads samples with cubic interpolation.
sfMsg3 :: Sf -> D -> Msg -> SE (Sig, Sig)
sfMsg3 = genSfMsg sfplay3

-- | Creates a midi instrument from sf2 sound font file.
-- The second argument is sustain in seconds.
-- Reads samples with linear interpolation.
-- Produces mono output.
sfMsgm :: Sf -> D -> Msg -> SE Sig
sfMsgm = genSfMsg sfplaym

-- | Creates a midi instrument from sf2 sound font file.
-- The second argument is sustain in seconds.
-- Reads samples with cubic interpolation.
-- Produces mono output.
sfMsg3m :: Sf -> D -> Msg -> SE Sig
sfMsg3m = genSfMsg sfplay3m

-- | Midi looper of the sf2 samples. 
-- The first arguments are: start, end, crossfade of the loop.
sfMsgLooper :: Sig -> Sig -> Sig -> Sf -> D -> Msg -> SE (Sig, Sig)
sfMsgLooper start end crossfade = genSfMsg $ 
    \vel key amp cps sf -> sflooper vel key amp cps sf start end crossfade

-----------------------------------------

-- | Reads sf2 samples at given midi velocity and key (both are from 0 to 127).
-- The second argument is sustain. Interpolation is linear.
sfKey :: Sf -> D -> D -> D -> (Sig, Sig)
sfKey = genSfKey sfplay

-- | Reads sf2 samples at given midi velocity and key (both are from 0 to 127).
-- The second argument is sustain. Interpolation is cubic.
sfKey3 :: Sf -> D -> D -> D -> (Sig, Sig)
sfKey3 = genSfKey sfplay3

-- | Reads sf2 samples at given midi velocity and key (both are from 0 to 127).
-- The second argument is sustain. Interpolation is linear. 
-- The output is mono.
sfKeym :: Sf -> D -> D -> D -> Sig
sfKeym = genSfKey sfplaym

-- | Reads sf2 samples at given midi velocity and key (both are from 0 to 127).
-- The second argument is sustain. Interpolation is cubic.
-- The output is mono.
sfKey3m :: Sf -> D -> D -> D -> Sig
sfKey3m = genSfKey sfplay3m

-- | Looper of the sf2 samples. 
-- The first arguments are: start, end, crossfade of the loop.
sfKeyLooper :: Sig -> Sig -> Sig -> Sf -> D -> D -> D -> (Sig, Sig)
sfKeyLooper start end crossfade = genSfKey $ 
    \vel key amp cps sf -> sflooper vel key amp cps sf start end crossfade

-----------------------------------------

-- | Reads sf2 samples with amplitude in (0, 1) and frequency in Hz. 
-- The interpolation is linear.
sfCps :: Sf -> D -> D -> D -> (Sig, Sig)
sfCps = genSfCps sfplay

-- | Reads sf2 samples with amplitude in (0, 1) and frequency in Hz. 
-- The interpolation is cubic.
sfCps3 :: Sf -> D -> D -> D -> (Sig, Sig)
sfCps3 = genSfCps sfplay3

-- | Reads sf2 samples with amplitude in (0, 1) and frequency in Hz. 
-- The interpolation is linear.
-- The output is mono.
sfCpsm :: Sf -> D -> D -> D -> Sig
sfCpsm = genSfCps sfplaym

-- | Reads sf2 samples with amplitude in (0, 1) and frequency in Hz. 
-- The interpolation is cubic.
-- The output is mono.
sfCps3m :: Sf -> D -> D -> D -> Sig
sfCps3m = genSfCps sfplay3m

-- | Looper of the sf2 samples. 
-- The first arguments are: start, end, crossfade of the loop.
sfCpsLooper :: Sig -> Sig -> Sig -> Sf -> D -> D -> D -> (Sig, Sig)
sfCpsLooper start end crossfade = genSfCps $ 
    \vel key amp cps sf -> sflooper vel key amp cps sf start end crossfade

----------------------------------------------

type SfFun a = D -> D -> Sig -> Sig -> Sf -> a

genSfMsg :: (SigSpace a, Sigs a) => SfFun a -> Sf -> D -> Msg -> SE a
genSfMsg play sf sustain msg = return $ mul env $ play (veloc msg) (notnum msg) 1 1 sf
    where env = sfEnv sustain (veloc msg / 127)

genSfKey :: SigSpace a => SfFun a -> Sf -> D -> D -> D -> a
genSfKey play sf sustain vel key = mul env $ play vel key 1 1 sf
    where env = sfEnv sustain (vel / 127)

genSfCps :: (Tuple a, SigSpace a) => SfFun a -> Sf -> D -> D -> D -> a
genSfCps play sf sustain amp cps = mul env $ play (127 * amp) (f2m cps) 1 (sig cps) sf `withD` 1 
    where env = sfEnv sustain amp 

sfEnv :: D -> D -> Sig
sfEnv sustain amp = sig frac * env
    where 
        frac = amp / 8000
        env  = linsegr [0, 0.007, 1] sustain 0

-- | frequency to midi
f2m :: D -> D
f2m cps = round' (12 * (log (cps / 220) / log 2) + 57)

