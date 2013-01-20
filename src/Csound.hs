module Csound(
    -- * Types
    Sig, D, S, I, Tab,
    MultiOut, CsdTuple,

    -- * Converters
    ToSig(sig), ar, kr,
    int, double, str,

    -- * Random
    SE, se, ses, se2,

    -- * Logic
    BoolSig, 
    module Data.Boolean,

    -- * Delay and feedback
    Buf, delayr, delayw, deltap, buf,

    -- * Midi
    Msg, midi, cpsmidi, ampmidi, pchbend, aftouch, ctrl7,

    -- * Opcodes

    -- ** Oscillators
    oscil, poscil, vco2,

    -- ** Envelops 
    linseg, linsegr, expseg, expsegr, lpshold, loopseg, looptseg,

    -- ** Panning
    pan2,

    -- ** Delay and feedback    

    -- ** Reverberation
    freeverb, reverbsc,

    -- ** Waveshaping
    table, tablei, distort,

    -- ** Convolution
    pconvolve, convolve, ftconv,

    -- ** Sound input
    diskin, diskin1, diskin2, diskin4, soundin, soundin2, 

    -- ** Random signals
    rand, noise, pinkish,

    -- ** Filters
    tone, atone, reson, comb,
    buthp, butlp, butbp, butbr, 
    moogladder, bqrez, vcomb,
    
    -- * Ftables
    gen,

    -- * Scores
    module Temporal.Music.Score,
    sco, Out, SigOut, Arg(..),

    -- * Rendering
    csd,

    -- * Arithmetic and Logic instances
    module Csound.Exp.Numeric,        
    module Csound.Exp.Logic,
    
    -- * Utils
    module Data.Default 
) where

import Data.Default
import Data.Boolean

import Temporal.Music.Score hiding(linseg, (!))

import Csound.Exp
import Csound.Exp.Cons
import Csound.Exp.Wrapper
import Csound.Opcode
import Csound.Exp.Numeric
import Csound.Exp.Logic
import Csound.Exp.Random
import Csound.Exp.Buf

import Csound.Render.Sco
import Csound.Render(csd)



