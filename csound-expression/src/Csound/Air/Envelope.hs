{-#  Language TypeFamilies, FlexibleInstances #-}
-- | Envelopes
module Csound.Air.Envelope (
    leg, xeg,

    -- ADSR with retrigger for mono-synths
    adsr140, trigTab, trigTabEvt,
    -- * Relative duration
    onIdur, lindur, expdur, linendur,
    onDur, lindurBy, expdurBy, linendurBy,

    -- * Faders
    fadeIn, fadeOut, fades, expFadeIn, expFadeOut, expFades, slope, expSlope,

    -- * Humanize
    HumanizeValue(..), HumanizeTime(..), HumanizeValueTime(..),
    hval, htime, hvalTime,

    -- * Looping envelopes

    -- ** Simple
    lpshold, loopseg, loopxseg, lpsholdBy, loopsegBy, loopxsegBy,
    holdSeq, linSeq, expSeq,
    linloop, exploop, sah, stepSeq,
    constSeq, triSeq, sqrSeq, sawSeq, isawSeq, xsawSeq, ixsawSeq, isqrSeq, xtriSeq,
    pwSeq, ipwSeq, rampSeq, irampSeq, xrampSeq, ixrampSeq,
    adsrSeq, xadsrSeq, adsrSeq_, xadsrSeq_,

    -- ** Complex
    Seq, toSeq, onBeat, onBeats,

    seqConst, seqLin, seqExp,

    seqPw, iseqPw, seqSqr, iseqSqr,
    seqSaw, iseqSaw, xseqSaw, ixseqSaw, seqRamp, iseqRamp, seqTri, seqTriRamp,
    seqAdsr, xseqAdsr, seqAdsr_, xseqAdsr_,

    seqPat, seqAsc, seqDesc, seqHalf
) where

import Control.Monad
import Control.Applicative
import Data.List(intersperse)

import Temporal.Media hiding (rest)
import qualified Temporal.Media as T(Rest(..))

import Csound.Typed
import Csound.Typed.Opcode hiding (lpshold, loopseg, loopxseg, release)
import qualified Csound.Typed.Opcode as C(lpshold, loopseg, loopxseg)
import Csound.Typed.Plugins(adsr140)
import Csound.Control.Evt(evtToTrig)

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
onDur dt xs = case xs of
    a:b:as -> a : b * dt : onDur dt as
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

-- | Slope envelope. It stays at zero for a given time then it raises to 1 for thre given time.
-- The function is usefull to delay the LFO.
--
-- > slope zeroTime rizeTime
slope :: D -> D -> Sig
slope dt1 dt2 = linseg [0, dt1, 0, dt2, 1 ]

-- | Exponential slope (See the function @slope@).
expSlope :: D -> D -> Sig
expSlope dt1 dt2 = linseg [0.001, dt1, 0.001, dt2, 1 ]

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
            _ : dt : rest -> dt + sumDts rest
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
                    [_] -> 0
                    _:b:rest -> b + go rest

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


sawList :: [Sig] -> [Sig]
sawList xs = case xs of
    []  -> []
    [a] -> a : 1 : 0 : []
    a:rest -> a : 1 : 0 : 0 : sawList rest

isawList :: [Sig] -> [Sig]
isawList xs = case xs of
    []  -> []
    [a] -> 0 : 1 : a : []
    a:rest -> 0 : 1 : a : 0 : isawList rest

triList :: [Sig] -> [Sig]
triList xs = case xs of
    [] -> [0, 0]
    a:rest -> 0 : 1 : a : 1 : triList rest

pwList :: Sig -> [Sig] -> [Sig]
pwList k xs = case xs of
    []   -> []
    a:as -> a : k : 0 : (1 - k) : pwList k as

ipwList :: Sig -> [Sig] -> [Sig]
ipwList k xs = case xs of
    []   -> []
    a:as -> 0 : k : a : (1 - k) : ipwList k as

rampList :: Sig -> Sig -> [Sig] -> [Sig]
rampList a1 duty xs = case xs of
    [] -> []
    [a] -> 0.5 * a : d1 : a : d1 : 0.5 * a : d2 : 0 : d2 : 0.5 * a1 : []
    a:as -> 0.5 * a : d1 : a : d1 : 0.5 * a : d2 : 0 : d2 : rampList a1 duty as
    where
        d1 = duty / 2
        d2 = (1 - duty) / 2

irampList :: Sig -> Sig -> [Sig] -> [Sig]
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


-- | The seq is a type for step sequencers.
-- The step sequencer is a monophonic control signal.
-- Most often step sequencer is a looping segment of
-- some values. It's used to create bas lines or conrtrol the frequency of
-- the filter in dub or trance music. There are simple functions
-- for creation of step sequencers defined in the module "Csound.Air.Envelope".
--
-- Basically the step sequence is a list of pairs:
--
-- >  [(valA, durA), (valB, durB), (valC, durC)]
--
-- each pair defines a segment of height valN that lasts for durN.
-- The sequence is repeated with the given frequency. Each segment
-- has certain shape. It can be a constant or line segment or
-- fragment of square wave or fragment of an adsr envelope.
-- There are many predefined functions.
--
-- With Seq we can construct control signals in very flexible way.
-- We can use the score composition functions for creation of sequences.
-- We can use @mel@ for sequencing of individual steps, we can use @str@
-- for stretching the sequence in time domain, we can delay with @del@.
--
-- Here is an example:
--
-- > dac $ tri $ seqConst [str 0.25 $ mel [440, 220, 330, 220], 110] 1
--
-- We can see how the function @str@ was used to make a certain segment faster.
-- There are numerical instaces for Seq. Bt it defines only functions @fronInteger@ and
-- @fromRational@.
newtype Seq = Seq { unSeq :: [Seq1] }

data Seq1 = Rest {
        seq1Dur :: Sig }
    | Seq1 {
          seq1Dur :: Sig
        , _seq1Val :: Sig
    }

type instance DurOf Seq = Sig

instance Duration Seq where
    dur (Seq as) = sum $ fmap seq1Dur as

instance T.Rest Seq where
    rest t = Seq [Rest t]

instance Delay Seq where
    del t a = mel [T.rest t, a]

instance Melody Seq where
    mel as = Seq $ as >>= unSeq

instance Stretch Seq where
    str t (Seq as) = Seq $ fmap (updateDur t) as
        where updateDur k a = a { seq1Dur = k * seq1Dur a }

-- | Creates a
toSeq :: Sig -> Seq
toSeq a = Seq [Seq1 1 a]

-- | Squashes a sequence to a single beat.
onBeat :: Seq -> Seq
onBeat a = str (1 / dur a) a

-- | Squashes a sequence to a single beat and then stretches to the given value.
onBeats :: Sig -> Seq -> Seq
onBeats k = str k . onBeat

instance Num Seq where
    fromInteger n = toSeq $ fromInteger n
    (+) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined

instance Fractional Seq where
    fromRational = toSeq . fromRational
    (/) = undefined

-------------------------------------------------

seqGen0 :: ([Sig] -> Sig -> Sig) -> (Sig -> Sig -> [Sig]) -> [Seq] -> Sig -> Sig
seqGen0 loopFun segFun as = loopFun (renderSeq0 segFun $ mel as)

seqGen1 :: ([Sig] -> Sig -> Sig) -> (Sig -> Sig -> [Sig]) -> [Seq] -> Sig -> Sig
seqGen1 loopFun segFun as = loopFun (renderSeq1 segFun $ mel as)

simpleSeq0, simpleSeq1 :: ([Sig] -> Sig -> Sig) -> [Seq] -> Sig -> Sig

simpleSeq0 loopFun = seqGen0 loopFun $ \dt val -> [val, dt]
simpleSeq1 loopFun = seqGen0 loopFun $ \dt val -> [val, dt]

seq1, seqx :: (Sig -> Sig -> [Sig]) -> [Seq] -> Sig -> Sig

seq1 = seqGen1 loopseg
seqx = seqGen1 loopxseg

-- | A sequence of constant segments.
seqConst :: [Seq] -> Sig -> Sig
seqConst = simpleSeq0 lpshold

-- | A linear sequence.
seqLin :: [Seq] -> Sig -> Sig
seqLin = simpleSeq1 loopseg

-- | An exponential sequence.
seqExp :: [Seq] -> Sig -> Sig
seqExp = simpleSeq1 loopxseg

-------------------------------------------------
-- square

-- | The sequence of pulse width waves.
-- The first argument is a duty cycle (ranges from 0 to 1).
seqPw :: Sig -> [Seq] -> Sig -> Sig
seqPw k = seqGen0 lpshold $ \dt val -> [val, dt * k, 0, dt * (1 - k)]

-- | The sequence of inversed pulse width waves.
iseqPw :: Sig -> [Seq] -> Sig -> Sig
iseqPw k = seqGen0 lpshold $ \dt val -> [0, dt * k, val, dt * (1 - k)]

-- | The sequence of square waves.
seqSqr :: [Seq] -> Sig -> Sig
seqSqr = seqPw 0.5

-- | The sequence of inversed square waves.
iseqSqr :: [Seq] -> Sig -> Sig
iseqSqr = iseqPw 0.5

-- saw

saw1 :: Num a => a -> a -> [a]
saw1  dt val = [val, dt, 0, 0]

isaw1 :: Num a => a -> a -> [a]
isaw1 dt val = [0, dt, val, 0]

-- | The sequence of sawtooth waves.
seqSaw :: [Seq] -> Sig -> Sig
seqSaw = seq1 saw1

-- | The sequence of inversed sawtooth waves.
iseqSaw :: [Seq] -> Sig -> Sig
iseqSaw = seq1 isaw1

-- | The sequence of exponential sawtooth waves.
xseqSaw :: [Seq] -> Sig -> Sig
xseqSaw = seqx saw1

-- | The sequence of inversed exponential sawtooth waves.
ixseqSaw :: [Seq] -> Sig -> Sig
ixseqSaw = seqx isaw1

-- | The sequence of ramp  functions. The first argument is a duty cycle.
seqRamp :: Sig -> [Seq] -> Sig -> Sig
seqRamp k = seq1 $ \dt val -> [val, k * dt, 0, (1 - k) * dt, 0, 0]

-- | The sequence of inversed ramp  functions. The first argument is a duty cycle.
iseqRamp :: Sig -> [Seq] -> Sig -> Sig
iseqRamp k = seq1 $ \dt val -> [0, k * dt, val, (1 - k) * dt, 0, 0]

-- tri

-- | The sequence of triangular waves.
seqTri :: [Seq] -> Sig -> Sig
seqTri = seqTriRamp 0.5

-- | The sequence of ramped triangular waves.
seqTriRamp :: Sig -> [Seq] -> Sig -> Sig
seqTriRamp k = seq1 $ \dt val -> [0, dt * k, val, dt * (1 - k)]

-- adsr

adsr1 :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> [Sig]
adsr1 a d s r dt val = [0, a * dt, val, d * dt, s * val, (1 - a - r), s * val, r * dt ]

adsr1_ :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> [Sig]
adsr1_ a d s r restSig dt val = [0, a * dt, val, d * dt, s * val, (1 - a - r - restSig), s * val, r * dt, 0, restSig ]

-- | The sequence of ADSR-envelopes.
--
-- > seqAdsr att dec sus rel
--
-- It has to be:
--
-- > att + dec + sus_time + rel == 1
seqAdsr :: Sig -> Sig -> Sig -> Sig -> [Seq] -> Sig -> Sig
seqAdsr a d s r = seq1 (adsr1 a d s r)

-- | The sequence of exponential ADSR-envelopes.
xseqAdsr :: Sig -> Sig -> Sig -> Sig -> [Seq] -> Sig -> Sig
xseqAdsr a d s r = seqx (adsr1 a d s r)

-- | The sequence of ADSR-envelopes with rest at the end.
--
-- > seqAdsr att dec sus rel rest
--
-- It has to be:
--
-- > att + dec + sus_time + rel + rest == 1

seqAdsr_ :: Sig -> Sig -> Sig -> Sig -> Sig -> [Seq] -> Sig -> Sig
seqAdsr_ a d s r restSig = seq1 (adsr1_ a d s r restSig)

-- | The sequence of exponential ADSR-envelopes with rest at the end.
xseqAdsr_ :: Sig -> Sig -> Sig -> Sig -> Sig -> [Seq] -> Sig -> Sig
xseqAdsr_ a d s r restSig = seqx (adsr1_ a d s r restSig)

-------------------------------------------------

renderSeq0 :: (Sig -> Sig -> [Sig]) -> Seq -> [Sig]
renderSeq0 f (Seq as) = as >>= phi
    where
        phi x = case x of
            Seq1 dt val -> f dt val
            Rest dt     -> [0, dt]

renderSeq1 :: (Sig -> Sig -> [Sig]) -> Seq -> [Sig]
renderSeq1 f (Seq as) = as >>= phi
    where
        phi x = case x of
            Seq1 dt val -> f dt val
            Rest dt     -> [0, dt, 0, 0]

-------------------------------------------------

genSeqPat :: (Int -> [Double]) -> [Int] -> Seq
genSeqPat g ns = mel (ns >>= f)
    where f n
            | n <= 0 = []
            | n == 1 = [1]
            | otherwise = fmap (toSeq . sig . double) $ g n

-- | Function for creation of accented beats.
-- The steady beat pattern of accents is repeated.
-- The first argument describes the list of integers.
-- Each integer is a main beat and the length of the beat.
-- We can create a typical latino beat:
--
-- > dac $ mul (seqSaw [seqPat [3, 3, 2]] 1) white
seqPat :: [Int] -> Seq
seqPat ns = mel (ns >>= f)
    where f n
            | n <= 0 = []
            | n == 1 = [1]
            | otherwise = [1, T.rest $ sig $ int $ n - 1]

rowDesc :: Int -> [Double]
rowDesc n = [1, 1 - recipN .. recipN ]
    where recipN = 1/ fromIntegral n

-- | It's like @seqPat@ but inplace of rests it fills the gaps with
-- segments descending in value.
--
-- > dac $ mul (seqSaw [seqDesc [3, 3, 2]] 1) white
seqDesc :: [Int] -> Seq
seqDesc = genSeqPat rowDesc

-- | It's like @seqPat@ but inplace of rests it fills the gaps with
-- segments ascending in value.
--
-- > dac $ mul (seqSaw [seqAsc [3, 3, 2]] 1) white
seqAsc :: [Int] -> Seq
seqAsc = genSeqPat (\n -> let xs = rowDesc n in head xs : reverse (tail xs))

-- | It's like @seqPat@ but inplace of rests it fills the gaps with 0.5s.
--
-- > dac $ mul (seqSaw [seqHalf [3, 3, 2]] 1) white
seqHalf :: [Int] -> Seq
seqHalf = genSeqPat $ (\n -> 1 : take (n - 1) (repeat 0.5))

-------------------------------------------------
-- humanizers

-- | Alias for @humanVal@.
hval :: HumanizeValue a => Sig -> a -> HumanizeValueOut a
hval = humanVal

-- | Alias for @humanTime@.
htime :: HumanizeTime a => Sig -> a -> HumanizeTimeOut a
htime = humanTime

-- | Alias for @humanValTime@.
hvalTime :: HumanizeValueTime a => Sig -> Sig -> a -> HumanizeValueTimeOut a
hvalTime = humanValTime

-- value

-- | A function transformer (decorator). We can transform an envelope producer
-- so that all values are sumed with some random value. The amplitude of the
-- random value is given with the first argument.
--
-- It can transform linseg, expseg, sequence producers and simplified sequence producers.
--
-- An example:
--
-- > dac $ mul (humanVal 0.1 sqrSeq [1, 0.5, 0.2, 0.1] 1) $ white
--
-- As you can see it transforms the whole function. So we don't need for extra parenthesis.
class HumanizeValue a where
    type HumanizeValueOut a :: *
    humanVal :: Sig -> a -> HumanizeValueOut a

rndVal :: Sig -> Sig -> Sig -> SE Sig
rndVal cps dr val = fmap (+ val) $ randh dr cps

rndValD :: Sig -> D -> SE D
rndValD dr val = fmap (+ val) $ random (- (ir dr)) (ir dr)

instance HumanizeValue ([Seq] -> Sig -> Sig) where
    type HumanizeValueOut ([Seq] -> Sig -> Sig) = [Seq] -> Sig -> SE Sig
    humanVal dr f = \sq cps -> fmap (\x -> f x cps) (mapM (humanSeq cps) sq)
        where
            humanSeq cps (Seq as) = fmap Seq $ forM as $ \x -> case x of
                Rest _      -> return x
                Seq1 dt val -> fmap (Seq1 dt) $ rndVal cps dr val

instance HumanizeValue ([Sig] -> Sig -> Sig) where
    type HumanizeValueOut ([Sig] -> Sig -> Sig) = [Sig] -> Sig -> SE Sig
    humanVal dr f = \sq cps -> fmap (\x -> f x cps) (mapM (humanSig cps) sq)
        where humanSig cps val = rndVal cps dr val

instance HumanizeValue ([D] -> Sig) where
    type HumanizeValueOut ([D] -> Sig) = [D] -> SE Sig
    humanVal dr f = \xs -> fmap f $ mapM human1 $ zip [0 ..] xs
        where human1 (n, a)
                    | mod n 2 == (1 :: Int) = rndValD dr a
                    | otherwise             = return a

instance HumanizeValue ([D] -> D -> Sig) where
    type HumanizeValueOut ([D] -> D -> Sig) = [D] -> D -> SE Sig
    humanVal dr f = \xs release -> fmap (flip f release) $ mapM human1 $ zip [0 ..] xs
        where human1 (n, a)
                    | mod n 2 == (1 :: Int) = rndValD dr a
                    | otherwise             = return a

-- time

-- | A function transformer (decorator). We can transform an envelope producer
-- so that all durations are sumed with some random value. The amplitude of the
-- random value is given with the first argument.
--
-- It can transform linseg, expseg, sequence producers and simplified sequence producers.
--
-- An example:
--
-- > dac $ mul (humanTime 0.1 sqrSeq [1, 0.5, 0.2, 0.1] 1) $ white
--
-- As you can see it transforms the whole function. So we don't need for extra parenthesis.
class HumanizeTime a where
    type HumanizeTimeOut a :: *
    humanTime :: Sig -> a -> HumanizeTimeOut a

instance HumanizeTime ([Seq] -> Sig -> Sig) where
    type HumanizeTimeOut ([Seq] -> Sig -> Sig) = [Seq] -> Sig -> SE Sig
    humanTime dr f = \sq cps -> fmap (\x -> f x cps) (mapM (humanSeq cps) sq)
        where
            humanSeq cps (Seq as) = fmap Seq $ forM as $ \x -> case x of
                Rest dt     -> fmap Rest $ rndVal cps dr dt
                Seq1 dt val -> fmap (flip Seq1 val) $ rndVal cps dr dt

instance HumanizeTime ([D] -> Sig) where
    type HumanizeTimeOut ([D] -> Sig) = [D] -> SE Sig
    humanTime dr f = \xs -> fmap f $ mapM human1 $ zip [0 ..] xs
        where human1 (n, a)
                    | mod n 2 == (0 :: Int) = rndValD dr a
                    | otherwise             = return a

instance HumanizeTime ([D] -> D -> Sig) where
    type HumanizeTimeOut ([D] -> D -> Sig) = [D] -> D -> SE Sig
    humanTime dr f = \xs release -> liftA2 f (mapM human1 $ zip [0 ..] xs) (rndValD dr release)
        where human1 (n, a)
                    | mod n 2 == (0 :: Int) = rndValD dr a
                    | otherwise             = return a

-- value & time

-- | A function transformer (decorator). We can transform an envelope producer
-- so that all values and durations are sumed with some random value. The amplitude of the
-- random value is given with the first two arguments.
--
-- It can transform linseg, expseg, sequence producers and simplified sequence producers.
--
-- An example:
--
-- > dac $ mul (humanValTime 0.1 0.1 sqrSeq [1, 0.5, 0.2, 0.1] 1) $ white
--
-- As you can see it transforms the whole function. So we don't need for extra parenthesis.
class HumanizeValueTime a where
    type HumanizeValueTimeOut a :: *
    humanValTime :: Sig -> Sig -> a -> HumanizeValueTimeOut a

instance HumanizeValueTime ([Seq] -> Sig -> Sig) where
    type HumanizeValueTimeOut ([Seq] -> Sig -> Sig) = [Seq] -> Sig -> SE Sig
    humanValTime drVal drTime f = \sq cps -> fmap (\x -> f x cps) (mapM (humanSeq cps) sq)
        where
            humanSeq cps (Seq as) = fmap Seq $ forM as $ \x -> case x of
                Rest dt     -> fmap Rest $ rndVal cps drTime dt
                Seq1 dt val -> liftA2 Seq1 (rndVal cps drTime dt) (rndVal cps drVal val)

instance HumanizeValueTime ([D] -> Sig) where
    type HumanizeValueTimeOut ([D] -> Sig) = [D] -> SE Sig
    humanValTime drVal drTime f = \xs -> fmap f $ mapM human1 $ zip [0 ..] xs
        where human1 (n, a)
                    | mod n 2 == (1 :: Int) = rndValD drVal  a
                    | otherwise             = rndValD drTime a

instance HumanizeValueTime ([D] -> D -> Sig) where
    type HumanizeValueTimeOut ([D] -> D -> Sig) = [D] -> D -> SE Sig
    humanValTime drVal drTime f = \xs release -> liftA2 f (mapM human1 $ zip [0 ..] xs) (rndValD drTime release)
        where human1 (n, a)
                    | mod n 2 == (1 :: Int) = rndValD drVal  a
                    | otherwise             = rndValD drTime a


-----------------------------------------------------
-- Trigger envelopes

-- | Triggers the table based envelope when the trigger signal equals to 1
-- and plays for dur seconds:
--
-- > trigTab table dur trigger
trigTab :: Tab -> Sig -> Sig -> Sig
trigTab ifn kdur ktrig =
    tablei (lineto ktrig (kdur * delay1 ktrig)) ifn `withD` 1


-- | Triggers the table based envelope when the something happens on the event stream
-- and plays for dur seconds:
--
-- > trigTabEvt table dur trigger
trigTabEvt :: Tab -> Sig -> Evt a -> Sig
trigTabEvt ifn kdur ktrig = trigTab ifn kdur (evtToTrig ktrig)

