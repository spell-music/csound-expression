-- | Envelopes
module Csound.Air.Envelope (
    leg, xeg,
    -- * Relative duration
    onIdur, lindur, expdur, linendur,
    onDur, lindurBy, expdurBy, linendurBy,    
    -- * Looping envelopes   
    lpshold, loopseg, loopxseg, lpsholdBy, loopsegBy, loopxsegBy,
    linloop, exploop, sah, stepSeq, 
    triSeq, sqrSeq, sawSeq, isawSeq, xsawSeq, ixsawSeq, isqrSeq, xtriSeq,

    -- * Faders
    fadeIn, fadeOut, fades, expFadeIn, expFadeOut, expFades

) where

import Data.List(intersperse)

import Csound.Typed
import Csound.Typed.Opcode hiding (lpshold, loopseg, loopxseg)
import qualified Csound.Typed.Opcode as C(lpshold, loopseg, loopxseg)
import Csound.Air.Wave
import Csound.Tab(lins, exps, gp)
import Csound.Air.Wave(oscBy)

-- | Linear adsr envelope generator with release
--
-- > leg attack decay sustain release
leg :: D -> D -> D -> D -> Sig
leg = madsr

-- | Exponential adsr envelope generator with release
--
-- > xeg attack decay sustain release
xeg :: D -> D -> D -> D -> Sig
xeg a d s r = mxadsr a d (s + 0.00001) r

-- | Makes time intervals relative to the note's duration. So that:
--
-- > onIdur [a, t1, b, t2, c]
--
-- becomes: 
--
-- > [a, t1 * idur, b, t2 * idur, c]
onIdur :: [D] -> [D]
onIdur = onDur idur

-- | Makes time intervals relative to the note's duration. So that:
--
-- > onDur dt [a, t1, b, t2, c]
--
-- becomes: 
--
-- > [a, t1 * dt, b, t2 * dt, c]
onDur :: D -> [D] -> [D]
onDur dur xs = case xs of
    a:b:as -> a : b * dur : onDur dur as
    _ -> xs

-- | The opcode 'Csound.Opcode.linseg' with time intervals 
-- relative to the total duration of the note.
lindur :: [D] -> Sig
lindur = linseg . onIdur

-- | The opcode 'Csound.Opcode.expseg' with time intervals 
-- relative to the total duration of the note.
expdur :: [D] -> Sig
expdur = expseg . onIdur

-- | The opcode 'Csound.Opcode.linseg' with time intervals 
-- relative to the total duration of the note given by the user.
lindurBy :: D -> [D] -> Sig
lindurBy dt = linseg . onDur dt

-- | The opcode 'Csound.Opcode.expseg' with time intervals 
-- relative to the total duration of the note given by the user.
expdurBy :: D -> [D] -> Sig
expdurBy dt = expseg . onDur dt

-- | The opcode 'Csound.Opcode.linen' with time intervals relative to the total duration of the note. Total time is set to the value of idur.
--
-- > linendur asig rise decay
linendur :: Sig -> D -> D -> Sig
linendur = linendurBy idur

-- | The opcode 'Csound.Opcode.linen' with time intervals relative to the total duration of the note. Total time is set to the value of
-- the first argument.
--
-- > linendurBy dt asig rise decay
linendurBy :: D -> Sig -> D -> D -> Sig
linendurBy dt asig ris dec = linen asig (ris * dt) dt (dec * dt)

        
-- | Fades in with the given attack time.
fadeIn :: D -> Sig
fadeIn att = linseg [0, att, 1]

-- | Fades out with the given attack time.
fadeOut :: D -> Sig
fadeOut dec = linsegr [1] dec 0
        
-- | Fades in by exponent with the given attack time.
expFadeIn :: D -> Sig
expFadeIn att = expseg [0.0001, att, 1]

-- | Fades out by exponent with the given attack time.
expFadeOut :: D -> Sig
expFadeOut dec = expsegr [1] dec 0.0001

-- | A combination of fade in and fade out.
--
-- > fades attackDuration decayDuration
fades :: D -> D -> Sig
fades att dec = fadeIn att * fadeOut dec

-- | A combination of exponential fade in and fade out.
--
-- > expFades attackDuration decayDuration
expFades :: D -> D -> Sig
expFades att dec = expFadeIn att * expFadeOut dec

-- The step sequencer. It takes the weights of constant steps and the frequency of repetition.
-- It outputs the piecewise constant function with given values. Values are equally spaced
-- and repeated with given rate.
stepSeq :: [Sig] -> Sig -> Sig
stepSeq as = lpshold (intersperseEnd 1 [1] as)

-- | Sample and hold cyclic signal. It takes the list of
--
-- > [a, dta, b, dtb, c, dtc, ...]
--
-- the a, b, c, ... are values of the constant segments
--
-- the dta, dtb, dtc, are durations in seconds of constant segments.
--
-- The period of the repetition equals to the sum of all durations.
sah :: [Sig] -> Sig
sah as = stepSeq as (1 / period)
    where 
        period = sumDts as

        sumDts xs = case xs of
            a : dt : rest -> dt + sumDts rest
            _ -> 0

-- | It's just like @linseg@ but it loops over the envelope.
linloop :: [Sig] -> Sig
linloop = genLoop loopseg . (++ [0])

-- | It's just like @expseg@ but it loops over the envelope.
exploop :: [Sig] -> Sig
exploop = genLoop loopxseg . (++ [0])

genLoop :: ([Sig] -> Sig -> Sig) -> [Sig] -> Sig
genLoop f as = f (tfmList as) (1 / len)
    where
        tfmList xs = case xs of
            [] -> []
            [a] -> [a]
            a:b:rest -> a : (b/len) : tfmList rest

        len = go as
            where
                go xs = case xs of
                    []  -> 0
                    [a] -> 0
                    a:b:rest -> b + go rest

-- | Step sequencer with unipolar triangle.
triSeq :: [Sig] -> Sig -> Sig
triSeq as cps = genSeq loopseg triList as (2 * cps)

-- | Step sequencer with unipolar square.
sqrSeq :: [Sig] -> Sig -> Sig
sqrSeq = genSeq stepSeq (intersperseEnd 0 [0])

-- | Step sequencer with unipolar sawtooth.
sawSeq :: [Sig] -> Sig -> Sig
sawSeq = genSeq loopseg sawList

-- | Step sequencer with unipolar inveted square.
isqrSeq :: [Sig] -> Sig -> Sig
isqrSeq = genSeq stepSeq ((0 : ) . intersperseEnd 0 [])

-- | Step sequencer with unipolar inveted sawtooth.
isawSeq :: [Sig] -> Sig -> Sig
isawSeq = genSeq loopseg isawList

-- | Step sequencer with unipolar exponential sawtooth.
xsawSeq :: [Sig] -> Sig -> Sig
xsawSeq = genSeq loopxseg sawList

-- | Step sequencer with unipolar inverted exponential sawtooth.
ixsawSeq :: [Sig] -> Sig -> Sig
ixsawSeq = genSeq loopxseg isawList

-- | Step sequencer with unipolar exponential triangle.
xtriSeq :: [Sig] -> Sig -> Sig
xtriSeq as cps = genSeq loopxseg triList as (2 * cps)

sawList xs = case xs of
    []  -> []           
    a:rest -> a : 1 : 0 : 0 : sawList rest
        
isawList xs = case xs of
    []  -> []           
    a:rest -> 0 : 1 : a : 0 : isawList rest

triList xs = case xs of
    [] -> [0, 0]
    a:rest -> 0 : 1 : a : 1 : triList rest 

------------------------------------------------------------------

genSeq :: ([Sig] -> Sig -> Sig) -> ([Sig] -> [Sig]) -> [Sig] -> Sig -> Sig
genSeq mkSeq go as cps = mkSeq (go as) (cps / len)
    where len = sig $ int $ length as

intersperseEnd :: a -> [a] -> [a] -> [a]
intersperseEnd val end xs = case xs of
    [] -> end
    [a] -> a : end
    a:as -> a : val : intersperseEnd val end as 

------------------------------------------------------------------

-- | Looping sample and hold envelope. The first argument is the list of pairs:
--
-- > [a, durA, b, durB, c, durc, ...]
--
-- It's a list of values and durations. The durations are relative
-- to the period of repetition. The period is specified with the second argument.
-- The second argument is the frequency of repetition measured in Hz.
-- 
-- > lpshold valDurs frequency
lpshold :: [Sig] -> Sig -> Sig
lpshold as cps = C.lpshold cps 0 0 as

-- | Looping linear segments envelope. The first argument is the list of pairs:
--
-- > [a, durA, b, durB, c, durc, ...]
--
-- It's a list of values and durations. The durations are relative
-- to the period of repetition. The period is specified with the second argument.
-- The second argument is the frequency of repetition measured in Hz.
-- 
-- > loopseg valDurs frequency
loopseg :: [Sig] -> Sig -> Sig
loopseg as cps = C.loopseg cps 0 0 as

-- | Looping exponential segments envelope. The first argument is the list of pairs:
--
-- > [a, durA, b, durB, c, durc, ...]
--
-- It's a list of values and durations. The durations are relative
-- to the period of repetition. The period is specified with the second argument.
-- The second argument is the frequency of repetition measured in Hz.
-- 
-- > loopxseg valDurs frequency
loopxseg :: [Sig] -> Sig -> Sig
loopxseg as cps = C.loopxseg cps 0 0 as

-- | It's like lpshold but we can specify the phase of repetition (phase belongs to [0, 1]).
lpsholdBy :: D -> [Sig] -> Sig -> Sig
lpsholdBy phase as cps = C.lpshold cps 0 phase  as

-- | It's like loopseg but we can specify the phase of repetition (phase belongs to [0, 1]).
loopsegBy :: D -> [Sig] -> Sig -> Sig
loopsegBy phase as cps = C.loopseg cps 0 phase  as

-- | It's like loopxseg but we can specify the phase of repetition (phase belongs to [0, 1]).
loopxsegBy :: D -> [Sig] -> Sig -> Sig
loopxsegBy phase as cps = C.loopxseg cps 0 phase  as
