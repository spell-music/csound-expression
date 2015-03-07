-- | Envelopes
module Csound.Air.Envelope (
    leg, xeg,
    -- * Relative duration
    onIdur, lindur, expdur, linendur,
    onDur, lindurBy, expdurBy, linendurBy,    
    -- * Looping envelopes   
    lpshold, loopseg, loopxseg, lpsholdBy, loopsegBy, loopxsegBy,
    holdSeq, linSeq, expSeq,
    linloop, exploop, sah, stepSeq, 
    constSeq, triSeq, sqrSeq, sawSeq, isawSeq, xsawSeq, ixsawSeq, isqrSeq, xtriSeq,
    pwSeq, ipwSeq, rampSeq, irampSeq, xrampSeq, ixrampSeq,
    adsrSeq, xadsrSeq, adsrSeq_, xadsrSeq_,  

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
import Csound.Air.Filter(slide)

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

-- | Sample and hold sequence. It outputs the looping sequence of constan elements.
constSeq :: [Sig] -> Sig -> Sig
constSeq = genSeq stepSeq id 

-- | Step sequencer with unipolar triangle.
triSeq :: [Sig] -> Sig -> Sig
triSeq as cps = genSeq loopseg triList as cps

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
xtriSeq as cps = genSeq loopxseg triList as (cps)

-- | A sequence of unipolar waves with pulse width moulation (see upw).
-- The first argument is a duty cycle in range 0 to 1.
pwSeq :: Sig -> [Sig] -> Sig -> Sig
pwSeq duty = genSeq lpshold (pwList duty)

-- | A sequence of unipolar inverted waves with pulse width moulation (see upw).
-- The first argument is a duty cycle in range 0 to 1.
ipwSeq :: Sig -> [Sig] -> Sig -> Sig
ipwSeq duty = genSeq lpshold (ipwList duty)

-- | A sequence of unipolar triangle waves with ramp factor (see uramp).
-- The first argument is a ramp factor cycle in range 0 to 1.
rampSeq :: Sig -> [Sig] -> Sig -> Sig
rampSeq duty xs = genSeq loopseg (rampList (head xs) duty) xs

-- | A sequence of unipolar exponential triangle waves with ramp factor (see uramp).
-- The first argument is a ramp factor cycle in range 0 to 1.
xrampSeq :: Sig -> [Sig] -> Sig -> Sig
xrampSeq duty xs = genSeq loopxseg (rampList (head xs) duty) xs

-- | A sequence of unipolar inverted triangle waves with ramp factor (see uramp).
-- The first argument is a ramp factor cycle in range 0 to 1.
irampSeq :: Sig -> [Sig] -> Sig -> Sig
irampSeq duty xs = genSeq loopseg (irampList (head xs) duty) xs

-- | A sequence of unipolar inverted exponential triangle waves with ramp factor (see uramp).
-- The first argument is a ramp factor cycle in range 0 to 1.
ixrampSeq :: Sig -> [Sig] -> Sig -> Sig
ixrampSeq duty xs = genSeq loopxseg (irampList (head xs) duty) xs


sawList xs = case xs of
    []  -> []       
    [a] -> a : 1 : 0 : []
    a:rest -> a : 1 : 0 : 0 : sawList rest
        
isawList xs = case xs of
    []  -> []  
    [a] -> 0 : 1 : a : []
    a:rest -> 0 : 1 : a : 0 : isawList rest

triList xs = case xs of
    [] -> [0, 0]
    a:rest -> 0 : 1 : a : 1 : triList rest 

pwList k xs = case xs of
    []   -> []
    a:as -> a : k : 0 : (1 - k) : pwList k as

ipwList k xs = case xs of
    []   -> []
    a:as -> 0 : k : a : (1 - k) : ipwList k as

rampList a1 duty xs = case xs of
    [] -> []
    [a] -> 0.5 * a : d1 : a : d1 : 0.5 * a : d2 : 0 : d2 : 0.5 * a1 : []
    a:as -> 0.5 * a : d1 : a : d1 : 0.5 * a : d2 : 0 : d2 : rampList a1 duty as  
    where 
        d1 = duty / 2
        d2 = (1 - duty) / 2

irampList a1 duty xs = case xs of
    [] -> []
    [a] -> 0.5 * a : d1 : 0 : d1 : 0.5 * a : d2 : a : d2 : 0.5 * a1 : []
    a:as -> 0.5 * a : d1 : 0 : d1 : 0.5 * a : d2 : a : d2 : rampList a1 duty as  
    where 
        d1 = duty / 2
        d2 = (1 - duty) / 2


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

smooth :: Sig -> Sig
smooth = flip portk 0.001

fixEnd :: [Sig] -> [Sig]
fixEnd = ( ++ [0])

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
lpshold as cps = smooth $ C.lpshold cps 0 0 as

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
loopseg as cps = smooth $ C.loopseg cps 0 0 (fixEnd as)

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
loopxseg as cps = smooth $ C.loopxseg cps 0 0 (fixEnd as)

-- | It's like lpshold but we can specify the phase of repetition (phase belongs to [0, 1]).
lpsholdBy :: D -> [Sig] -> Sig -> Sig
lpsholdBy phase as cps = smooth $ C.lpshold cps 0 phase  as

-- | It's like loopseg but we can specify the phase of repetition (phase belongs to [0, 1]).
loopsegBy :: D -> [Sig] -> Sig -> Sig
loopsegBy phase as cps = smooth $ C.loopseg cps 0 phase (fixEnd as)

-- | It's like loopxseg but we can specify the phase of repetition (phase belongs to [0, 1]).
loopxsegBy :: D -> [Sig] -> Sig -> Sig
loopxsegBy phase as cps = smooth $ C.loopxseg cps 0 phase (fixEnd as)

-- | The looping ADSR envelope.
--
-- > xadsrSeq attack decay sustain release weights frequency
--
-- The sum of attack, decay, sustain and release time durations 
-- should be equal to one.
adsrSeq :: Sig -> Sig -> Sig -> Sig -> [Sig] -> Sig -> Sig
adsrSeq a d s r = linSeq (adsrList a d s r)

-- | The looping exponential ADSR envelope. there is a fifth segment
-- at the end of the envelope during which the envelope equals to zero.
--
-- > xadsrSeq attack decay sustain release weights frequency
--
-- The sum of attack, decay, sustain and release time durations 
-- should be equal to one.
xadsrSeq :: Sig -> Sig -> Sig -> Sig -> [Sig] -> Sig -> Sig
xadsrSeq a d s r = expSeq (adsrList a d s r)

-- | The looping ADSR envelope with the rest at the end.
--
-- > adsrSeq attack decay sustain release rest weights frequency
--
-- The sum of attack, decay, sustain, release and rest time durations 
-- should be equal to one.
adsrSeq_ :: Sig -> Sig -> Sig -> Sig -> Sig -> [Sig] -> Sig -> Sig
adsrSeq_ a d s r rest = linSeq (adsrList_ a d s r rest)

-- | The looping exponential ADSR envelope. there is a fifth segment
-- at the end of the envelope during which the envelope equals to zero.
--
-- > xadsrSeq_ attack decay sustain release rest weights frequency
--
-- The sum of attack, decay, sustain, release and rest time durations 
-- should be equal to one.
xadsrSeq_ :: Sig -> Sig -> Sig -> Sig -> Sig -> [Sig] -> Sig -> Sig
xadsrSeq_ a d s r rest = expSeq (adsrList_ a d s r rest)

adsrList :: Sig -> Sig -> Sig -> Sig -> [Sig]
adsrList a d s r = [0, a, 1, d, s, 1 - (a + d + r), s, r, 0]

adsrList_ :: Sig -> Sig -> Sig -> Sig -> Sig -> [Sig]
adsrList_ a d s r rest = [0, a, 1, d, s, 1 - (a + d + r + rest), s, r, 0, rest, 0]

-- | The looping sequence of constant segments.
--
-- > linSeg [a, durA, b, durB, c, durC, ...] [scale1, scale2, scale3] cps
--
-- The first argument is the list that specifies the shape of the looping wave.
-- It's the alternating values and durations of transition from one value to another.
-- The durations are relative to the period. So that lists
--
-- > [0, 0.5, 1, 0.5, 0]  and [0, 50, 1, 50, 0]
--
-- produce the same results. The second list is the list of scales for subsequent periods.
-- Every value in the period is scaled with values from the second list.
-- The last argument is the rate of repetition (Hz).
holdSeq :: [Sig] -> [Sig] -> Sig -> Sig
holdSeq = genSegSeq lpshold

-- | The looping sequence of linear segments.
--
-- > linSeg [a, durA, b, durB, c, durC, ...] [scale1, scale2, scale3] cps
--
-- The first argument is the list that specifies the shape of the looping wave.
-- It's the alternating values and durations of transition from one value to another.
-- The durations are relative to the period. So that lists
--
-- > [0, 0.5, 1, 0.5, 0]  and [0, 50, 1, 50, 0]
--
-- produce the same results. The second list is the list of scales for subsequent periods.
-- Every value in the period is scaled with values from the second list.
-- The last argument is the rate of repetition (Hz).
linSeq :: [Sig] -> [Sig] -> Sig -> Sig
linSeq = genSegSeq loopseg

-- | The looping sequence of exponential segments.
--
-- > expSeg [a, durA, b, durB, c, durC, ...] [scale1, scale2, scale3] cps
--
-- The first argument is the list that specifies the shape of the looping wave.
-- It's the alternating values and durations of transition from one value to another.
-- The durations are relative to the period. So that lists
--
-- > [0, 0.5, 1, 0.5, 0]  and [0, 50, 1, 50, 0]
--
-- produce the same results. The second list is the list of scales for subsequent periods.
-- Every value in the period is scaled with values from the second list.
-- The last argument is the rate of repetition (Hz).
expSeq :: [Sig] -> [Sig] -> Sig -> Sig
expSeq = genSegSeq loopxseg

genSegSeq :: ([Sig] -> Sig -> Sig) -> [Sig] -> [Sig] -> Sig -> Sig
genSegSeq mkSeg shape weights cps = mkSeg (groupSegs $ fmap (scaleVals shape) weights) (cps / len)
    where 
        len = sig $ int $ length weights
        scaleVals xs k = case xs of
            [] -> []
            [a] -> [a * k]
            a:da:rest -> (a * k) : da : scaleVals rest k    

        groupSegs :: [[Sig]] -> [Sig]
        groupSegs as = concat $ intersperse [0] as
