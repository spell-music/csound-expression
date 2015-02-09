-- | Envelopes
module Csound.Air.Envelope (
    leg, xeg,
    -- * Relative duration
    onIdur, lindur, expdur, linendur,
    onDur, lindurBy, expdurBy, linendurBy,    
    -- * Looping envelopes
    oscLins, oscElins, oscExps, oscEexps, oscLine, 
    linloop, exploop, sah, stepSeq,
    sawSeq, isawSeq, sawExpSeq, isawExpSeq, sqrSeq, isqrSeq,

    -- * Faders
    fadeIn, fadeOut, fades, expFadeIn, expFadeOut, expFades,
) where

import Data.List(intersperse)

import Csound.Typed
import Csound.Typed.Opcode
import Csound.Air.Misc
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

-- | Loops over line segments with the given rate.
--
-- > oscLins [a, durA, b, durB, c, durC ..] cps
--
-- where 
--
-- * @a@, @b@, @c@ ... -- values
--
-- * durA, durB, durC -- durations of the segments relative to the current frequency.
oscLins :: [D] -> Sig -> Sig
oscLins points cps = loopseg cps 0 0 (fmap sig points) 

-- | Loops over equally spaced line segments with the given rate.
--
-- > oscElins [a, b, c] === oscLins [a, 1, b, 1, c]
oscElins :: [D] -> Sig -> Sig
oscElins points = oscLins (intersperse 1 points)

-- | 
--
-- > oscLine a b cps
--
-- Goes from @a@ to @b@ and back by line segments. One period is equal to @2\/cps@ so that one period is passed by @1\/cps@ seconds.
oscLine :: D -> D -> Sig -> Sig
oscLine a b cps = oscElins [a, b, a] (cps / 2)

-- | Loops over exponential segments with the given rate.
--
-- > oscLins [a, durA, typeA, b, durB, typeB, c, durC, typeC ..] cps
--
-- where 
--
-- * @a@, @b@, @c@ ... -- values
--
-- * durA, durB, durC -- durations of the segments relative to the current frequency.
--
-- * typeA, typeB, typeC, ... -- shape of the envelope. If the value is 0 then the shap eis linear; otherwise it is an concave exponential (positive type) or a convex exponential (negative type).
oscExps :: [D] -> Sig -> Sig
oscExps points cps = looptseg cps 0 (fmap sig points)

-- | Loops over equally spaced exponential segments with the given rate.
--
-- > oscLins [a, typeA, b, typeB, c, typeC ..] === oscLins [a, 1, typeA, b, 1, typeB, c, 1, typeC ..]
oscEexps :: [D] -> Sig -> Sig
oscEexps points = oscExps (insertOnes points)
    where insertOnes xs = case xs of
            a:b:as  -> a:1:b:insertOnes as
            _       -> xs

-- The step sequencer. It takes the length of the period and the list of doubles.
-- It outputs the piecewise constant function with given values. Values are equally spaced
-- and repeated with given rate.
stepSeq :: Sig -> [D] -> Sig
stepSeq dt as = oscLins (mkList as) (1 / dt)
    where 
        mkList xs = case xs of
            []   -> [a1]
            b:bs -> b : len : b : 0 : mkList bs
        len = 1 / (int $ length as)
        a1 = case as of
            a:_ -> a
            _   -> 0

-- | Sample and hold cyclic signal. It takes the list of
--
-- > [a, dta, b, dtb, c, dtc, ...]
--
-- the a, b, c, ... are values of the constant segments
--
-- the dta, dtb, dtc, are durations in seconds of constant segments.
--
-- The period of the repetition equals to the sum of all durations.
sah :: [D] -> Sig
sah = linloop . tfmList
    where 
        tfmList xs = case xs of
            [] -> []
            [a] -> [a]
            a:da:rest -> a : da : a : 0 : tfmList rest

-- | It's just like @linseg@ but it loops over the envelope.
linloop :: [D] -> Sig
linloop = genLoop oscLins

-- | It's just like @expseg@ but it loops over the envelope.
exploop :: [D] -> Sig
exploop = genLoop oscExps

genLoop :: ([D] -> Sig -> Sig) -> [D] -> Sig
genLoop f as = f (tfmList as) (1 / sig len)
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

genSeq :: ([Double] -> Tab) -> ([Double] -> [Double]) -> [Double] -> Sig -> Sig
genSeq mkTab mkList as cps = oscBy (skipNorm $ gp $ mkTab $ mkList as) (cps / (sig $ int $ length as))

-- | A sawtooth step sequencer. The unipolar sawtooth wave with scaled teeth. The values for teeth 
-- are given in the first argument. They are repeated.
sawSeq :: [Double] -> Sig -> Sig  
sawSeq = genSeq lins mkList
    where 
        mkList xs = case xs of
            []   -> []
            a:as -> a : 1 : 0 : 0 : mkList as 

-- | An inverted sawtooth step sequencer. It's like @sawSeq@ but a tooth starts from 0 and goes to the 1.
-- It's useful for creation of reversed envelopes.
isawSeq :: [Double] -> Sig -> Sig  
isawSeq = genSeq lins mkList
    where 
        mkList xs = case xs of
            []   -> []
            a:as -> 0 : 1 : a : 0 : mkList as 

expVal a 
    | abs a < 0.0001 = (if (a < 0 ) then (-1) else 1) * 0.0001
    | otherwise      = a

-- | An exponential saw step sequencer. It's like @sawSeq@ but
-- the rise is exponential instead of linear.
sawExpSeq :: [Double] -> Sig -> Sig  
sawExpSeq = genSeq exps mkList
    where 
        mkList xs = case fmap expVal xs of
            []   -> []
            a:as -> a : 1 : 0.0001 : 0 : mkList as 

-- | An inverted exponential saw step sequencer. It's like @sawSeq@ but
-- the rise is exponential instead of linear.
isawExpSeq :: [Double] -> Sig -> Sig  
isawExpSeq = genSeq exps mkList
    where 
        mkList xs = case fmap expVal xs of
            []   -> []
            a:as -> 0.0001 : 1 : a : 0 : mkList as 

sqrSeq :: [Double] -> Sig -> Sig  
sqrSeq = genSeq lins mkList
    where
        mkList xs = case xs of
            []   -> []
            a:as -> a : 0.5 : a : 0 : 0 : 0.5 : 0 : 0 : mkList as 

isqrSeq :: [Double] -> Sig -> Sig  
isqrSeq = genSeq lins mkList
    where
        mkList xs = case xs of
            []   -> []
            a:as -> 0 : 0.5 : 0 : 0 : a : 0.5 : a : 0 : mkList as 

