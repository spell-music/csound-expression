-- | Basic signal processing
module Csound.Opcode.Basic(
    -----------------------------------------------------
    -- * Global constants
    idur, zeroDbfs, getSampleRate, getBlockSize,

    -----------------------------------------------------
    -- * Oscillators and phasors

    -- ** Standard Oscillators
    oscils, poscil, poscil3, oscil, oscili, oscil3, oscil1i,

    -- ** Dynamic Sprectrum Oscillators
    buzz, gbuzz, mpulse, vco, vco2,  

    -- ** Phasors
    phasor, syncphasor,

    -----------------------------------------------------
    -- * Random and Noise generators    
    rand, randi, randh, rnd31, random, randomi, randomh, pinkish, noise, pinkish', noise', jitter, jitter2, jspline,

    -----------------------------------------------------
    -- * Envelopes
    linseg, expseg, transeg, linsegr, expsegr, transegr,
    lpshold, loopseg, loopxseg, looptseg, loopsegp,
    linen, linenr, envlpx, 
    
    -----------------------------------------------------
    -- * Delays

    -- ** Fixed delays
    delaySig, delay1, delayk,
    
    -- ** Delay lines
    delayr, delayw, deltap, deltapi, deltap3, deltapx, deltapxw,
    
    -- Variable delays
    vdelay, vdelay3, vdelayx, vdelayxw, vdel_k,

    -----------------------------------------------------
    -- * Filters

    -- ** Low Pass Filters
    tone, butlp,

    -- ** High Pass Filters
    atone, buthp,

    -- ** Band Pass And Resonant Filters
    reson, butbp,

    -- ** Band Reject Filters
    areson, butbr,

    -- ** Filters For Smoothing Control Signals
    port, portk,

    -- ** Other filters
    moogladder, vcomb, bqrez, comb, moogvcf, moogvcf2,
    resonr, resonx, resony, resonz, 

    -----------------------------------------------------
    -- * Reverb
    freeverb, reverbsc, reverb, reverb2, nreverb, babo, 

    -----------------------------------------------------
    -- * Signal Measurement, Dynamic Processing, Sample Level Operations

    -- ** Amplitude Measurement And Following
    rms, balance, follow, follow2, peak, max_k,

    -- ** Pitch Estimation
    ptrack, pitch, pitchamdf, tempest,

    -- ** Tempo Estimation

    -- ** Dynamic Processing
    compress, dam, clip, 

    -- ** Sample Level Operations
    limit, samphold, vaget, vaset,  

    -----------------------------------------------------
    -- * Spatialization

    -- ** Panning
    pan, pan2,

    -- ** Binaural / HTRF
    hrtfstat, hrtfmove, hrtfmove2,

    -----------------------------------------------------
    -- * Control 
    
    times, timek,
    metro, trigger, 
    metroE, changedE, triggerE, 
    xtratim, ihold, turnoff
     
) where

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.SE
import Csound.Exp.Event
import Csound.LowLevel

import Csound.Render.Channel(zeroDbfs, clip, ihold, turnoff, changed)

-- | Reads @p3@-argument for the current instrument.
idur :: D
idur = p 3
        
-- | Reads @sr@ value.
getSampleRate :: D
getSampleRate = (setRate Ir :: E -> D) $ readOnlyVar (VarVerbatim Ir "sr")

-- | Reads @ksmps@ value.
getBlockSize :: D
getBlockSize = (setRate Ir :: E -> D) $ readOnlyVar (VarVerbatim Ir "ksmps")

-----------------------------------------------------
-- Standard Oscillators


-- | Simple, fast sine oscillator, that uses only one multiply, and two add operations to generate one sample of output, and does not require a function table. 
--
-- > ares oscils iamp, icps, iphs [, iflg]
--
-- doc: <http://www.csounds.com/manual/html/oscils.html>
oscils :: Iamp -> Icps -> D -> Sig
oscils = opc3 "oscils" [(a, [i, i, i])]

oscGen :: Name -> Sig -> Sig -> Tab -> Sig 
oscGen name = opc3 name [
    (a, [x, x, i, i]),
    (k, [k, k, i, i])]

-- | oscil reads table ifn sequentially and repeatedly at a frequency xcps. The amplitude is scaled by xamp. 
--
-- > ares oscil xamp, xcps, ifn [, iphs]
-- > kres oscil kamp, kcps, ifn [, iphs]
--
-- doc: <http://www.csounds.com/manual/html/oscil.html>
oscil :: Amp -> Cps -> Tab -> Sig
oscil = oscGen "oscil"

-- |  oscili reads table ifn sequentially and repeatedly at a frequency xcps. The amplitude is scaled by xamp. Linear interpolation is applied for table look up from internal phase values. 
--
-- > ares oscili xamp, xcps, ifn [, iphs]
-- > kres oscili kamp, kcps, ifn [, iphs]
--
-- doc: <http://www.csounds.com/manual/html/oscili.html>
oscili :: Amp -> Cps -> Tab -> Sig
oscili = oscGen "oscili"

-- |  oscil3 reads table ifn sequentially and repeatedly at a frequency xcps. The amplitude is scaled by xamp. Cubic interpolation is applied for table look up from internal phase values. 
--
-- > ares oscil3 xamp, xcps, ifn [, iphs]
-- > kres oscil3 kamp, kcps, ifn [, iphs]
--
-- doc: <http://www.csounds.com/manual/html/oscil3.html>

oscil3 :: Amp -> Cps -> Tab -> Sig
oscil3 = oscGen "oscil3"

-- | High precision oscillator. 
--
-- > ares poscil xamp, xcps, ifn [, iphs]
-- > kres poscil kamp, kcps, ifn [, iphs]
--
-- doc: <http://www.csounds.com/manual/html/poscil.html>

poscil :: Amp -> Cps -> Tab -> Sig
poscil = oscGen "poscil"

-- | High precision oscillator with cubic interpolation. 
--
-- > ares poscil3 xamp, xcps, ifn [, iphs]
-- > kres poscil3 kamp, kcps, ifn [, iphs]
--
-- doc: <http://www.csounds.com/manual/html/poscil3.html>

poscil3 :: Amp -> Cps -> Tab -> Sig
poscil3 = oscGen "poscil3"

-- | Accesses table values by incremental sampling with linear interpolation. 
--
-- > kres oscil1i idel, kamp, idur, ifn
--
-- doc: <http://www.csounds.com/manual/html/oscil1i.html>
oscil1i :: D -> Amp -> D -> Tab -> Sig
oscil1i = opc4 "oscil1i" [(k, [i, k, i, i])]

-----------------------------------------------------
-- Dynamic Sprectrum Oscillators

-- buzz, gbuzz, mpulse, vco, vco2  

-- |  Output is a set of harmonically related sine partials. 
--
-- > ares buzz xamp, xcps, knh, ifn [, iphs]
--
-- doc: <http://www.csounds.com/manual/html/buzz.html> 

buzz :: Amp -> Cps -> Sig -> Tab -> Sig
buzz = opc4 "buzz" [(a, [x, x, k, i, i])]

-- |  Output is a set of harmonically related cosine partials. 
--
-- > ares gbuzz xamp, xcps, knh, klh, kmul, ifn [, iphs]
--
-- doc: <http://www.csounds.com/manual/html/gbuzz.html> 

gbuzz :: Amp -> Cps -> Sig -> Sig -> Sig -> Tab -> Sig
gbuzz = opc6 "gbuzz" [(a, [x, x, k, k, k, i, i])]

-- | Generates a set of impulses of amplitude kamp separated by kintvl seconds (or samples if kintvl is negative). The first impulse is generated after a delay of ioffset seconds. 
--
-- > ares mpulse kamp, kintvl [, ioffset]
--
-- doc: <http://www.csounds.com/manual/html/mpulse.html> 

mpulse :: Amp -> Sig -> Sig
mpulse = opc2 "mpulse" [(a, [k, k, i])]

-- | Implementation of a band limited, analog modeled oscillator, based on integration of band limited impulses. vco can be used to simulate a variety of analog wave forms. 
--
-- > ares vco xamp, xcps, iwave, kpw [, ifn] [, imaxd] [, ileak] [, inyx] \
-- >     [, iphs] [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/vco.html> 

vco :: Amp -> Cps -> D -> Sig -> Sig
vco = opc4 "vco" [(a, [x, x, i, k] ++ is 6)]

-- | vco2 is similar to vco. But the implementation uses pre-calculated tables of band-limited waveforms (see also GEN30) 
-- rather than integrating impulses. This opcode can be faster than vco (especially if a low control-rate is used) and also 
-- allows better sound quality. Additionally, there are more waveforms and oscillator phase can be modulated at k-rate. 
-- The disadvantage is increased memory usage. For more details about vco2 tables, see also vco2init and vco2ft.
--
-- > ares vco2 kamp, kcps [, imode] [, kpw] [, kphs] [, inyx]
--
-- doc: <http://www.csounds.com/manual/html/vco2.html> 

vco2 :: Amp -> Cps -> Sig
vco2 = opc2 "vco2" [(a, [k, k, i, k, k, i])]


-----------------------------------------------------
-- Phasors

-- | Produce a normalized moving phase value. 
--
-- > ares phasor xcps [, iphs]
-- > kres phasor kcps [, iphs]
--
-- doc: <http://www.csounds.com/manual/html/phasor.html> 

phasor :: Cps -> Sig 
phasor = opc1 "phasor" [
    (a, [x, i]),
    (k, [x, i])]

-- | Produces a moving phase value between zero and one and an extra impulse output ("sync out") whenever its phase value
-- crosses or is reset to zero. The phase can be reset at any time by an impulse on the "sync in" parameter. 
--
-- > aphase, asyncout syncphasor xcps, asyncin, [, iphs]
--
-- doc: <http://www.csounds.com/manual/html/syncphasor.html> 

syncphasor :: Sig -> Sig -> (Sig, Sig)
syncphasor = mopc2 "syncphasor" ([a, a], [x, a, i])


-----------------------------------------------------
-- Random

-- | Output is a controlled random number series between -amp and +amp 
--
-- > ares rand xamp [, iseed] [, isel] [, ioffset]
-- > kres rand xamp [, iseed] [, isel] [, ioffset]
--
-- doc: <http://www.csounds.com/manual/html/rand.html> 

rand :: Amp -> SE Sig
rand asig = se $ opc1 "rand" [
    (a, x:rest),
    (k, k:rest)] asig
    where rest = is 3

-- | Generates a controlled random number series with interpolation between each new number. 
--
-- > ares randi xamp, xcps [, iseed] [, isize] [, ioffset]
-- > kres randi kamp, kcps [, iseed] [, isize] [, ioffset]
--
-- doc: <http://www.csounds.com/manual/html/randi.html> 

randi :: Amp -> Sig -> SE Sig
randi = randiGen "randi"

-- | Generates random numbers and holds them for a period of time. 
--
-- > ares randh xamp, xcps [, iseed] [, isize] [, ioffset]
-- > kres randh kamp, kcps [, iseed] [, isize] [, ioffset]
--
-- doc: <http://www.csounds.com/manual/html/randh.html> 

randh :: Amp -> Sig -> SE Sig
randh = randiGen "randh"

randiGen :: Name -> Sig -> Sig -> SE Sig
randiGen name a1 a2 = se $ opc2 name [
    (a, x:x:rest),
    (k, k:k:rest)] a1 a2
    where rest = is 3

-- | 31-bit bipolar random opcodes with controllable distribution. These units are portable, i.e. using the same seed 
-- value will generate the same random sequence on all systems. The distribution of --generated random numbers can be varied at k-rate. 
--
-- > ax rnd31 kscl, krpow [, iseed]
-- > ix rnd31 iscl, irpow [, iseed]
-- > kx rnd31 kscl, krpow [, iseed]
--
-- doc: <http://www.csounds.com/manual/html/rnd31.html> 

rnd31 :: Sig -> Sig -> SE Sig
rnd31 a1 a2 = se $ opc2 "rnd31" [
    (a, [k, k, i]),
    (k, [k, k, i]),
    (i, [i, i, i])] a1 a2

-- | Generates is a controlled pseudo-random number series between min and max values. 
--
-- > ax random kscl, krpow
-- > ix random iscl, irpow
-- > kx random kscl, krpow
--
-- doc: <http://www.csounds.com/manual/html/random.html> 

random :: Sig -> Sig -> SE Sig
random a1 a2 = se $ opc2 "random" [
    (a, [k, k]),
    (k, [k, k]),
    (i, [i, i])] a1 a2

-- | Generates a user-controlled random number series with interpolation between each new number. 
--
-- > ares randomi kmin, kmax, xcps [,imode] [,ifirstval]
-- > kres randomi kmin, kmax, kcps [,imode] [,ifirstval]
--
-- doc: <http://www.csounds.com/manual/html/randomi.html> 
randomi :: Amp -> Amp -> Cps -> SE Sig
randomi a1 a2 a3 = se $ opc3 "randomi" [
    (a, [k, k, x, i, i]),
    (k, [k, k, k, i, i])] a1 a2 a3

-- | Generates random numbers with a user-defined limit and holds them for a period of time.
--
-- > ares randomh kmin, kmax, xcps [,imode] [,ifirstval]
-- > kres randomh kmin, kmax, kcps [,imode] [,ifirstval]
--
-- doc: <http://www.csounds.com/manual/html/randomh.html> 

randomh :: Amp -> Amp -> Cps -> SE Sig
randomh a1 a2 a3 = se $ opc3 "randomh" [
    (a, [k, k, x, i, i]),
    (k, [k, k, k, i, i])] a1 a2 a3

-- | Generates approximate pink noise (-3dB/oct response) by one of two different methods:
--
-- * a multirate noise generator after Moore, coded by Martin Gardner
--
-- * a filter bank designed by Paul Kellet
--
-- > ares pinkish xin [, imethod] [, inumbands] [, iseed] [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/pinkish.html> 

pinkish :: Sig -> SE Sig 
pinkish a1 = se $ opc1 "pinkish" [(a, x: replicate 4 i)] a1

-- | Unsafe version of the 'pinkish' opcode. Unsafe means that there can be possible alias
-- on the expression level. Two expressions with the same arguments can be unexpectedly
-- rendered as the same expression within one instrument. 

pinkish' :: Sig -> Sig
pinkish' = opc1 "pinkish" [(a, x: replicate 4 i)]

-- | A white noise generator with an IIR lowpass filter. 
--
-- > ares noise xamp, kbeta
--
-- doc: <http://www.csounds.com/manual/html/noise.html>
 
noise :: Amp -> Sig -> SE Sig
noise a1 a2 = se $ opc2 "noise" [(a, [x, k])] a1 a2

-- | Unsafe version of the 'noise' opcode. Unsafe means that there can be possible alias
-- on the expression level. Two expressions with the same arguments can be unexpectedly
-- rendered as the same expression within one instrument. 

noise' :: Amp -> Sig -> Sig
noise' = opc2 "noise" [(a, [x, k])]

-- | Generates a segmented line whose segments are randomly generated. 
--
-- > kout jitter kamp, kcpsMin, kcpsMax
--
-- doc: <http://www.csounds.com/manual/html/jitter.html>

jitter :: Sig -> Sig -> Sig -> SE Sig
jitter a1 a2 a3 = se $ opc3 "jitter" [(k, [k, k, k])] a1 a2 a3

-- | Generates a segmented line with user-controllable random segments.
--
-- > kout jitter2 ktotamp, kamp1, kcps1, kamp2, kcps2, kamp3, kcps3
--
-- doc: <http://www.csounds.com/manual/html/jitter2.html>

jitter2 :: Sig -> [Sig] -> SE Sig
jitter2 k1 kks = se $ opcs "jitter2" [(k, repeat k)] $ k1 : kks

-- | A jitter-spline generator.
--
-- > aout jspline xamp, kcpsMin, kcpsMax
-- > kout jspline kamp, kcpsMin, kcpsMax
--
-- doc: <http://www.csounds.com/manual/html/jspline.html>

jspline :: Sig -> Sig -> Sig -> SE Sig
jspline a1 a2 a3 = se $ opc3 "jspline" [(k, [k, k, k])] a1 a2 a3

--------------------------------------------------
-- envelopes

-- | Trace a series of line segments between specified points. 
--
-- > ares linseg ia, idur1, ib [, idur2] [, ic] [...]
-- > kres linseg ia, idur1, ib [, idur2] [, ic] [...]
--
-- doc: <http://www.csounds.com/manual/html/linseg.html>
linseg :: [D] -> Ksig
linseg = kr . opcs "linseg" [
    (a, repeat i),
    (k, repeat i)]

-- | Trace a series of line segments between specified points including a release segment. 
--
-- > ares linsegr ia, idur1, ib [, idur2] [, ic] [...], irel, iz
-- > kres linsegr ia, idur1, ib [, idur2] [, ic] [...], irel, iz
--
-- doc: <http://www.csounds.com/manual/html/linsegr.html>

linsegr :: [D] -> D -> D -> Ksig
linsegr xs relDur relVal = kr $ opcs "linsegr" ([
    (a, repeat i),
    (k, repeat i)]) (xs ++ [relDur, relVal])


-- | Trace a series of exponential segments between specified points.
--
-- > ares expseg ia, idur1, ib [, idur2] [, ic] [...]
-- > kres expseg ia, idur1, ib [, idur2] [, ic] [...]
--
-- doc: <http://www.csounds.com/manual/html/expseg.html>

expseg :: [D] -> Ksig
expseg = kr . opcs "expseg" [
    (a, repeat i),
    (k, repeat i)]

-- | Trace a series of exponential segments between specified points including a release segment. 
--
-- > ares expsegr ia, idur1, ib [, idur2] [, ic] [...], irel, iz
-- > kres expsegr ia, idur1, ib [, idur2] [, ic] [...], irel, iz
--
-- doc: <http://www.csounds.com/manual/html/expsegr.html>

expsegr :: [D] -> D -> D -> Ksig
expsegr xs relDur relVal = kr $ opcs "expsegr" ([
    (a, repeat i),
    (k, repeat i)]) (xs ++ [relDur, relVal])

-- | Constructs a user-definable envelope.
--
-- > ares transeg ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
-- > kres transeg ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
--
-- doc: <http://www.csounds.com/manual/html/transeg.html>
transeg :: [D] -> Ksig
transeg = kr . opcs "transeg" [ 
    (a, repeat i),
    (k, repeat i)]

-- | Constructs a user-definable envelope. It is the same as transeg, with an extended release segment.
--
-- > ares transeg ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
-- > kres transeg ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
--
-- doc: <http://www.csounds.com/manual/html/transegr.html>
transegr :: [D] -> D -> D -> Ksig
transegr xs relDur relVal = kr $ opcs "transegr" ([
    (a, repeat i),
    (k, repeat i)]) (xs ++ [relDur, relVal])

-- | Generate control signal consisting of held segments delimited 
-- by two or more specified points. The entire envelope is looped at kfreq rate. 
-- Each parameter can be varied at k-rate. 
--
-- > ksig lpshold kfreq, ktrig, ktime0, kvalue0  [, ktime1] [, kvalue1] \
-- >       [, ktime2] [, kvalue2] [...]
--
-- doc: <http://www.csounds.com/manual/html/lpshold.html>

lpshold :: Sig -> Sig -> D -> [Sig] -> Ksig
lpshold = mkLpsWithPhase "lpshold"

-- | Generate control signal consisting of linear segments delimited by two or
-- more specified points. The entire envelope is looped at kfreq rate.
-- Each parameter can be varied at k-rate. 
--
-- > ksig loopseg kfreq, ktrig, iphase, ktime0, kvalue0 [, ktime1] [, kvalue1] \
-- >       [, ktime2] [, kvalue2] [...]
--
-- doc: <http://www.csounds.com/manual/html/loopseg.html>
loopseg :: Sig -> Sig -> D -> [Sig] -> Ksig
loopseg = mkLpsWithPhase "loopseg"

-- | Generate control signal consisting of exponential segments delimited by two or
-- more specified points. The entire envelope is looped at kfreq rate.
-- Each parameter can be varied at k-rate.
--
-- > ksig loopxseg kfreq, ktrig, iphase, ktime0, kvalue0 [, ktime1] [, kvalue1] \
-- >       [, ktime2] [, kvalue2] [...]
--
-- doc: <http://www.csounds.com/manual/html/loopxseg.html>
loopxseg :: Sig -> Sig -> D -> [Sig] -> Ksig
loopxseg = mkLpsWithPhase "loopxseg"

-- | Generate control signal consisting of controllable exponential segments 
-- or linear segments delimited by two or more specified points. 
-- The entire envelope is looped at kfreq rate. Each parameter can be varied at k-rate. 
--
-- > ksig looptseg kfreq, ktrig, ktime0, kvalue0, ktype0, [, ktime1] [, kvalue1] [,ktype1] \
-- >       [, ktime2] [, kvalue2] [,ktype2] [...][, ktimeN] [, kvalueN]
--
-- doc: <http://www.csounds.com/manual/html/looptseg.html>
looptseg :: Sig -> Sig -> [Sig] -> Ksig
looptseg = mkLps "looptseg"

mkLps :: Name -> Sig -> Sig -> [Sig] -> Ksig
mkLps name kfreq ktrig kvals = kr $ opcs name signature $ kfreq:ktrig:kvals
    where signature = [(k, repeat k)]

mkLpsWithPhase :: Name -> Sig -> Sig -> D -> [Sig] -> Ksig
mkLpsWithPhase name kfreq ktrig iphase kvals = kr $ opcs name signature $ (toE kfreq) : (toE ktrig) : (toE iphase) : (fmap toE kvals)
    where signature = [(k, k : k : i : repeat k)]

-- | Generate control signal consisiting of linear segments delimited by 
-- two or more specified points. The entire envelope can be looped at 
-- time-variant rate. Each segment coordinate can also be varied at k-rate.
--
-- > ksig loopsegp  kphase, kvalue0, kdur0, kvalue1 \
-- >      [, kdur1, ... , kdurN-1, kvalueN]
--
-- doc: <http://www.csounds.com/manual/html/loopsegp.html>
loopsegp :: Sig -> [Sig] -> Ksig
loopsegp kphase xs = kr $ opcs "loopsegp" signature (kphase : xs) 
    where signature = [(k, repeat k)]

-- | Apply a stright line rise and decay pattern to an imput amp signal.
--
-- > kr linen kamp, iris, idur, idec
-- > ar linen xamp, iris, idur, idec
--
-- doc: <http://www.csounds.com/manual/html/linen.html>
linen :: Amp -> D -> D -> D -> Sig
linen a1 a2 a3 a4 = opc4 "linen" [
    (k, k:is 3),
    (a, x:is 3)] a1 a2 a3 a4
    
-- | Apply a stright line rise then an exponential decay decay while the note is extended in time.
--
-- > kr linenr kamp, iris, idur, iatdec
-- > ar linenr xamp, iris, idur, iatdec
--
-- doc: <http://www.csounds.com/manual/html/linenr.html>
linenr :: Amp -> D -> D -> D -> Sig
linenr a1 a2 a3 a4 = opc4 "linenr" [
    (k, k:is 3),
    (a, x:is 3)] a1 a2 a3 a4

-- | Apply an envelope consisting of 3 segments:
--
-- * stored function rise shape
--
-- * modified exponential \"pseudo steady state\" 
--
-- * exponential decay
--
-- > kr envlpx kamp, irise, idur, idec, ifn, iatss, iatdec, [ixmod]
-- > ar envlpx xamp, irise, idur, idec, ifn, iatss, iatdec, [ixmod]
--
-- doc: <http://www.csounds.com/manual/html/envlpx.html>
envlpx :: Amp -> D -> D -> D -> Tab -> D -> D -> Sig
envlpx a1 a2 a3 a4 a5 a6 a7 = opc7 "envlpx" [
    (k, k:is 8),
    (a, x:is 8)] a1 a2 a3 a4 a5 a6 a7

----------------------------------------------------
-- audio delays

-- | Delays an input signal by some time interval. 
-- Name is changed to prevent name collisions with 
-- score arrangment functions.
--
-- > ares delay asig, idlt [, iskip] 
--
-- doc: <http://www.csounds.com/manual/html/delay.html>
delaySig :: Sig -> D -> Sig
delaySig = opc2 "delay" [(a, [a, i, i])]

-- | Delays an input signal by one sample
--
-- > ares delay1 asig [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/delay1.html>
delay1 :: Sig -> Sig
delay1 = opc1 "delay1" [(a, [a, i])]

-- | k-rate delay opcodes 
--
-- > kr delayk   ksig, idel[, imode]
--
-- doc: <http://www.csounds.com/manual/html/delayk.html>
delayk :: Sig -> D -> Sig
delayk = opc2 "delayk" [(k, [k, i, i])]

-- | This is an interpolating variable time delay, it is not very different from the existing implementation (deltapi), it is only easier to use. 
--
-- > ares vdelay asig, adel, imaxdel [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/vdelay.html>
vdelay :: Sig -> Sig -> D -> Sig
vdelay = opc3 "vdelay" [(a, [a, a, i, i])]

-- | vdelay3 is experimental. It is the same as vdelay except that it uses cubic interpolation.  
--
-- > ares vdelay3 asig, adel, imaxdel [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/vdelay3.html>
vdelay3 :: Sig -> Sig -> D -> Sig
vdelay3 = opc3 "vdelay3" [(a, [a, a, i, i])]

-- | A variable delay opcode with high quality interpolation. 
--
-- > aout vdelayx ain, adl, imd, iws [, ist]
--
-- doc: <http://www.csounds.com/manual/html/vdelayx.html>
vdelayx :: Sig -> Sig -> D -> D -> Sig
vdelayx = opc4 "vdelayx" [(a, [a, a, i, i, i])]

-- | Variable delay opcodes with high quality interpolation. 
--
-- aout vdelayxw ain, adl, imd, iws [, ist]
--
-- doc: <http://www.csounds.com/manual/html/vdelayxw.html>
vdelayxw :: Sig -> Sig -> D -> D -> Sig
vdelayxw = opc4 "vdelayxw" [(a, [a, a, i, i, i])]

-- | Delays an input signal by one sample
--
-- > kr vdel_k   ksig, kdel, imdel[, imode]
--
-- doc: <http://www.csounds.com/manual/html/delay1.html>
vdel_k :: Sig -> Sig -> D -> Sig
vdel_k = opc3 "vdel_k" [(k, [k, k, i, i])]

----------------------------------------------------
-- delay lines

-- | Reads from an automatically established digital delay line. 
--
-- > ares delayr idlt [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/delayr.html>
delayr :: D -> SE Sig
delayr a1 = se $ opc1 "delayr" [(a, [i])] a1

-- | Writes the audio signal to a digital delay line. 
--
-- > delayw asig
--
-- doc: <http://www.csounds.com/manual/html/delayw.html>
delayw :: Sig -> SE ()
delayw a1 = se_ $ opc1 "delayw" [(x, [a])] a1

-- | Tap a delay line at variable offset times. 
--
-- > ares deltap kdlt
-- 
-- doc: <http://www.csounds.com/manual/html/deltap.html>
deltap :: Sig -> SE Sig
deltap a1 = se $ opc1 "deltap" [(a, [k])] a1

-- | Taps a delay line at variable offset times, uses interpolation. 
--
-- > ares deltapi xdlt
--
-- doc: <http://www.csounds.com/manual/html/deltapi.html>
deltapi :: Sig -> SE Sig
deltapi a1 = se $ opc1 "deltapi" [(a, [x])] a1

-- | Taps a delay line at variable offset times, uses cubic interpolation. 
--
-- > ares deltap3 xdlt
--
-- doc: <http://www.csounds.com/manual/html/deltap3.html>
deltap3 :: Sig -> SE Sig
deltap3 a1 = se $ opc1 "deltap3" [(a, [x])] a1

-- | deltapx is similar to deltapi or deltap3. However, it allows higher quality interpolation. This opcode can read from and write to a delayr/delayw delay line with interpolation. 
--
-- > aout deltapx adel, iwsize
--
-- doc: <http://www.csounds.com/manual/html/deltapx.html>
deltapx :: Sig -> D -> SE Sig
deltapx a1 a2 = se $ opc2 "deltapx" [(a, [a, i])] a1 a2

-- | deltapxw mixes the input signal to a delay line. This opcode can be mixed with reading units 
-- (deltap, deltapn, deltapi, deltap3, and deltapx) in any order; the actual delay time is the difference of
-- the read and write time. This opcode can read from and write to a delayr/delayw delay line with interpolation. 
--
-- > deltapxw ain, adel, iwsize
--
-- doc: <http://www.csounds.com/manual/html/deltapxw.html>
deltapxw :: Sig -> Sig -> D -> SE ()
deltapxw a1 a2 a3 = se_ $ opc3 "deltapxw" [(x, [a, a, i])] a1 a2 a3

---------------------------------------------------
-- filters

filterSignature1 :: [(Rate, [Rate])]
filterSignature1 = [(a, [a, k, i])]

mkFilter1 :: (Val a1, Val a2, Val b) => Name -> a1 -> a2 -> b
mkFilter1 name = opc2 name filterSignature1

filterSignature2 :: [(Rate, [Rate])]
filterSignature2 = [(a, [a, k, k, i])]

mkFilter2 :: (Val a1, Val a2, Val a3, Val b) => Name -> a1 -> a2 -> a3 -> b
mkFilter2 name = opc3 name filterSignature2


-- | A first-order recursive low-pass filter with variable frequency response.
--
-- tone is a 1 term IIR filter. Its formula is:
--
-- > yn = c1 * xn + c2 * yn-1
--
-- where
--
--  * b = 2 - cos(2 π hp/sr);
--
--  * c2 = b - sqrt(b2 - 1.0)
--
--  * c1 = 1 - c2
--
-- > ares tone asig, khp [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/tone.html>

tone :: Sig -> Sig -> Sig

-- | A hi-pass filter whose transfer functions are the complements of the tone opcode. 
--
-- > ares atone asig, khp [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/atone.html>
atone :: Sig -> Sig -> Sig

-- | Implementation of a second-order low-pass Butterworth filter. 
--
-- > ares butlp asig, kfreq [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/butterlp.html>
butlp :: Sig -> Sig -> Sig

-- | Implementation of second-order high-pass Butterworth filter.
--
-- > ares buthp asig, kfreq [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/butterhp.html>

buthp :: Sig -> Sig -> Sig

-- | A second-order resonant filter.
--
-- > ares reson asig, kcf, kbw [, iscl] [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/reson.html>
 
reson :: Sig -> Sig -> Sig -> Sig

-- |  A notch filter whose transfer functions are the complements of the reson opcode. 
--
-- > ares areson asig, kcf, kbw [, iscl] [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/areson.html>

areson :: Sig -> Sig -> Sig -> Sig

-- | Implementation of a second-order band-pass Butterworth filter. 
--
-- > ares butbp asig, kfreq, kband [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/butterbp.html>
butbp :: Sig -> Sig -> Sig -> Sig

-- | Implementation of a second-order band-reject Butterworth filter. 
--
-- > ares butbr asig, kfreq, kband [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/butterbr.html>
butbr :: Sig -> Sig -> Sig -> Sig

-- | Moogladder is an new digital implementation of the Moog ladder filter based on the work of 
-- Antti Huovilainen, described in the paper "Non-Linear Digital Implementation of the Moog Ladder Filter" (Proceedings of DaFX04, Univ of Napoli). 
-- This implementation is probably a more accurate digital representation of the original analogue filter.
--
-- > asig moogladder ain, kcf, kres[, istor]
--
-- doc: <http://www.csounds.com/manual/html/moogladder.html>
moogladder :: Sig -> Sig -> Sig -> Sig

tone  = mkFilter1 "tone"
atone = mkFilter1 "atone"
butlp = mkFilter1 "butlp"
buthp = mkFilter1 "buthp"

reson  = mkFilter2 "reson"
areson = mkFilter2 "areson"
butbp  = mkFilter2 "butbp"
butbr  = mkFilter2 "butbr"
moogladder = mkFilter2 "moogladder"

-- | A digital emulation of the Moog diode ladder filter configuration.
--
-- > ares moogvcf asig, xfco, xres [,iscale, iskip]
-- 
-- doc: <http://www.csounds.com/manual/html/moogvcf.html>

moogvcf :: Sig -> Sig -> Sig -> Sig
moogvcf = opc3 "moogvcf" [(a, [x, x, i, i])]

-- | A digital emulation of the Moog diode ladder filter configuration.
--
-- > ares moogvcf2 asig, xfco, xres [,iscale, iskip]
-- 
-- doc: <http://www.csounds.com/manual/html/moogvcf2.html>

moogvcf2 :: Sig -> Sig -> Sig -> Sig
moogvcf2 = opc3 "moogvcf2" [(a, [x, x, i, i])]


-- | Variably reverberates an input signal with a “colored” frequency response. 
--
-- > ares vcomb asig, krvt, xlpt, imaxlpt [, iskip] [, insmps]
--
-- doc: <http://www.csounds.com/manual/html/vcomb.html>
vcomb :: Sig -> Sig -> Sig -> D -> Sig
vcomb = opc4 "vcomb" [(a, [a, k, x, i, i, i])]

-- | A second-order multi-mode filter. 
--
-- > ares bqrez asig, xfco, xres [, imode] [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/bqrez.html>
bqrez :: Sig -> Sig -> Sig -> Sig
bqrez = opc3 "bqrez" [(a, [a, x, x, i, i])]

-- | Reverberates an input signal with a “colored” frequency response. 
--
-- > ares comb asig, krvt, ilpt [, iskip] [, insmps]
--
-- doc: <http://www.csounds.com/manual/html/comb.html>
comb :: Sig -> Sig -> D -> Sig
comb = opc3 "comb" [(a, [a, k, i, i, i])]

-- | Applies portamento to a step-valued control signal. 
--
-- > kres port ksig, ihtim [, isig]
--
-- doc: <http://www.csounds.com/manual/html/port.html>
port :: Sig -> D -> Sig
port = opc2 "port" [(k, [k, i, i])]

-- | Applies portamento to a step-valued control signal. 
--
-- > kres portk ksig, khtim [, isig]
--
-- doc: <http://www.csounds.com/manual/html/portk.html>
portk :: Sig -> Sig -> Sig
portk = opc2 "portk" [(k, [k, k, i])]

-- | Implementations of a second-order, two-pole two-zero bandpass filter 
-- with variable frequency response.
--
-- > ares resonr asig, kcf, kbw [, iscl] [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/resonr.html>
resonr :: Sig -> Sig -> Sig -> Sig
resonr = opc3 "resonr" [(a, [a, k, k, i, i])]

-- | resonx is equivalent to a filters consisting of
-- more layers of reson with the same arguments, 
-- serially connected. Using a stack of a larger 
-- number of filters allows a sharper cutoff. 
-- They are faster than using a larger number instances 
-- in a Csound orchestra of the old opcodes, because 
-- only one initialization and k- cycle are needed at 
-- time and the audio loop falls entirely inside the 
-- cache memory of processor.
--
-- > ares resonx asig, kcf, kbw [, inumlayer] [, iscl] [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/resonx.html>
resonx :: Sig -> Sig -> Sig -> Sig
resonx = opc3 "resonx" [(a, [a, k, k, i, i, i])]

-- | A bank of second-order bandpass filters, connected in parallel.
--
-- > ares resony asig, kbf, kbw, inum, ksep [, isepmode] [, iscl] [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/resony.html>
resony :: Sig -> Sig -> Sig -> D -> Sig -> Sig
resony = opc5 "resony" [(a, [k, k, i, k, i, i])]

-- | Implementations of a second-order, two-pole 
-- two-zero bandpass filter with variable frequency 
-- response.
--
-- > ares resonz asig, kcf, kbw [, iscl] [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/resonz.html>
resonz :: Sig -> Sig -> Sig -> Sig
resonz = opc3 "resonz" [(a, [a, k, k, i, i])]

---------------------------------------------------
-- reverberation

-- | freeverb is a stereo reverb unit based on Jezar's public domain C++ sources, composed of eight parallel 
-- comb filters on both channels, followed by four allpass units in series. The filters on the right channel 
-- are slightly detuned compared to the left channel in order to create a stereo effect.
--
-- > aoutL, aoutR freeverb ainL, ainR, kRoomSize, kHFDamp[, iSRate[, iSkip]] 
--
-- doc: <http://www.csounds.com/manual/html/freeverb.html>
freeverb :: Sig -> Sig -> Sig -> Sig -> (Sig, Sig)
freeverb = mopc4 "freeverb" ([a, a], [a, a, k, k, i, i])

-- | 8 delay line stereo FDN reverb, with feedback matrix based upon physical modeling scattering junction of 8 
-- lossless waveguides of equal characteristic impedance. Based on Csound orchestra version by Sean Costello. 
--
-- > aoutL, aoutR reverbsc ainL, ainR, kfblvl, kfco[, israte[, ipitchm[, iskip]]] 
--
-- doc: <http://www.csounds.com/manual/html/reverbsc.html>
reverbsc :: Sig -> Sig -> Sig -> Sig -> (Sig, Sig)
reverbsc = mopc4 "reverbsc" ([a, a], [a, a, k, k, i, i, i])

-- | Reverberates an input signal with a “natural room” frequency response. 
--
-- > ares reverb asig, krvt [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/reverb.html>
reverb :: Sig -> Sig -> Sig
reverb = opc2 "reverb" [(a, [a, k, i])]

-- | This is a reverberator consisting of 6 parallel comb-lowpass filters
-- being fed into series of 5 allpass filters.
--
-- > ares reverb2 asig, ktime, khdif
--
-- doc: <http://www.csounds.com/manual/html/reverb2.html>
reverb2 :: Sig -> Sig -> Sig -> Sig
reverb2 = opc3 "reverb2" [(a, [a, k, k])]

-- | This is a reverberator consisting of 6 parallel comb-lowpass filters being fed into a series 
-- of 5 allpass filters. nreverb replaces reverb2 (version 3.48) and so both opcodes are identical. 
--
-- > ares nreverb asig, ktime, khdif [, iskip] [,inumCombs] [, ifnCombs] \
-- >       [, inumAlpas] [, ifnAlpas]
--
-- doc: <http://www.csounds.com/manual/html/nreverb.html>
nreverb :: Sig -> Sig -> Sig -> Sig
nreverb = opc3 "nreverb" [(a, [a, k, k] ++ is 5)]

-- | babo stands for ball-within-the-box. It is a physical model reverberator based on the paper
-- by Davide Rocchesso "The Ball within the Box: a sound-processing metaphor", Computer Music Journal, Vol 19, N.4, pp.45-47, Winter 1995.
--
-- The resonator geometry can be defined, along with some response characteristics, the position of the listener within the resonator, and the position of the sound source. 
--
-- > a1, a2 babo asig, ksrcx, ksrcy, ksrcz, irx, iry, irz [, idiff] [, ifno]
--
-- doc: <http://www.csounds.com/manual/html/babo.html>
babo :: Sig -> Sig -> Sig -> Sig -> D -> D -> D -> (Sig, Sig)
babo = mopc7 "babo" ([a, a], [a, k, k, k] ++ is 5)

---------------------------------------------------
-- Amplitude Measurement And Following

-- | Determines the root-mean-square amplitude of an audio signal. It low-pass filters the actual value, to average in the manner of a VU meter. 
--
-- > kres rms asig [, ihp] [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/rms.html>
rms :: Sig -> Sig
rms = opc1 "rms" [(k, [a, i, i])]

-- | The rms power of asig can be interrogated, set, or adjusted to match that of a comparator signal. 
--
-- > ares balance asig, acomp [, ihp] [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/balance.html>
balance :: Sig -> Sig -> Sig
balance = opc2 "balance" [(a, [a, a, i, i])]

-- | Envelope follower unit generator. 
--
-- > ares follow asig, idt
--
-- doc: <http://www.csounds.com/manual/html/follow.html>
follow :: Sig -> D -> Sig
follow = opc2 "follow" [(a, [a, i])]

-- | A controllable envelope extractor using the algorithm attributed to Jean-Marc Jot. 
--
-- > ares follow2 asig, katt, krel
--
-- doc: <http://www.csounds.com/manual/html/follow2.html>
follow2 :: Sig -> Sig -> Sig -> Sig
follow2 = opc3 "follow2" [(a, [a, k, k])]

-- | These opcodes maintain the output k-rate variable as the peak absolute level so far received. 
--
-- > kres peak asig
-- > kres peak ksig
--
-- doc: <http://www.csounds.com/manual/html/peak.html>
peak :: Sig -> Sig
peak = opc1 "peak" [(k, [x])]

-- | max_k outputs the local maximum (or minimum) value of the incoming asig signal, checked in the time interval between ktrig has become true twice. 
--
-- > knumkout max_k asig, ktrig, itype
--
-- doc: <http://www.csounds.com/manual/html/max_k.html>
max_k :: Sig -> Sig -> D -> Sig
max_k = opc3 "max_k" [(k, [a, k, i])]

-------------------------------------------------
-- Pitch Estimation

-- | ptrack takes an input signal, splits it into ihopsize blocks and using a STFT method, extracts an estimated 
-- pitch for its fundamental frequency as well as estimating the total amplitude of the signal in dB, relative to
-- full-scale (0dB). The method implies an analysis window size of 2*ihopsize samples (overlaping by 1/2 window), which
-- has to be a power-of-two, between 128 and 8192 (hopsizes between 64 and 4096). Smaller windows will give better time
-- precision, but worse frequency accuracy (esp. in low fundamentals).This opcode is based on an original algorithm by M. Puckette. 
--
-- > kcps, kamp ptrack asig, ihopsize[,ipeaks]
--
-- doc: <http://www.csounds.com/manual/html/ptrack.html>
ptrack :: Sig -> D -> (Sig, Sig)
ptrack = mopc2 "ptrack" ([k, k], [a, i, i])

-- | Using the same techniques as spectrum and specptrk, pitch tracks the pitch of the signal in octave point decimal form, and amplitude in dB. 
--
-- > koct, kamp pitch asig, iupdte, ilo, ihi, idbthresh [, ifrqs] [, iconf] \
-- >       [, istrt] [, iocts] [, iq] [, inptls] [, irolloff] [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/pitch.html>
pitch :: Sig -> D -> D -> D -> D -> (Sig, Sig) 
pitch = mopc5 "pitch" ([k, k], a:is 12)

-- | Follows the pitch of a signal based on the AMDF method (Average Magnitude Difference Function). Outputs pitch and amplitude 
-- tracking signals. The method is quite fast and should run in realtime. This technique usually works best for monophonic signals. 
--
-- > kcps, krms pitchamdf asig, imincps, imaxcps [, icps] [, imedi] \
-- >       [, idowns] [, iexcps] [, irmsmedi]
--
-- doc: <http://www.csounds.com/manual/html/pitchamdf.html>
pitchamdf :: Sig -> D -> D -> (Sig, Sig)
pitchamdf = mopc3 "pitchamdf" ([k, k], a:is 8)

-- | Estimate the tempo of beat patterns in a control signal. 
--
-- > ktemp tempest kin, iprd, imindur, imemdur, ihp, ithresh, ihtim, ixfdbak, \
-- >       istartempo, ifn [, idisprd] [, itweek]
--
-- doc: <http://www.csounds.com/manual/html/tempest.html>
tempest :: Sig -> D -> D -> D -> D -> D -> D -> D -> D -> Tab -> Sig
tempest = opc10 "tempest" [(k, k:is 11)]

-------------------------------------------------
-- Dynamic Processing

-- | This unit functions as an audio compressor, limiter, expander, or noise gate, using either soft-knee or hard-knee 
-- mapping, and with dynamically variable performance characteristics. It takes two audio input signals, aasig and acsig,
-- the first of which is modified by a running analysis of the second. Both signals can be the same, or the first can be 
-- modified by a different controlling signal.
--
-- compress first examines the controlling acsig by performing envelope detection. This is directed by two control
-- values katt and krel, defining the attack and release time constants (in seconds) of the detector. The detector 
-- rides the peaks (not the RMS) of the control signal. Typical values are .01 and .1, the latter usually being similar
-- to ilook.
-- 
-- The running envelope is next converted to decibels, then passed through a mapping function to determine what compresser
-- action (if any) should be taken. The mapping function is defined by four decibel control values. These are given as 
-- positive values, where 0 db corresponds to an amplitude of 1, and 90 db corresponds to an amplitude of 32768. 
--
-- > ar compress aasig, acsig, kthresh, kloknee, khiknee, kratio, katt, krel, ilook
--
-- doc: <http://www.csounds.com/manual/html/compress.html>
compress :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> Sig
compress = opc9 "compress" [(a, [a, a, k, k, k, k, k, k, i])]

-- | This opcode dynamically modifies a gain value applied to the input sound ain by comparing its power level to a given 
-- threshold level. The signal will be compressed/expanded with different factors regarding that it is over or under the threshold. 
--
-- > ares dam asig, kthreshold, icomp1, icomp2, irtime, iftime
--
-- doc: <http://www.csounds.com/manual/html/dam.html>
dam :: Sig -> Sig -> D -> D -> D -> D -> Sig
dam = opc6 "dam" [(a, a:k:is 4)]

-------------------------------------------------
-- Sample Level Operations

-- | Sets the lower and upper limits of the value it processes. 
--
-- > ares limit asig, klow, khigh
-- > ires limit isig, ilow, ihigh
-- > kres limit ksig, klow, khigh
--
-- doc: <http://www.csounds.com/manual/html/limit.html>
limit :: Sig -> Sig -> Sig -> Sig
limit = opc3 "limit" [
    (a, [a, k, k]),
    (k, [k, k, k]),
    (i, [i, i, i])]

-- | Performs a sample-and-hold operation on its input. 
--
-- > ares samphold asig, agate [, ival] [, ivstor]
-- > kres samphold ksig, kgate [, ival] [, ivstor]
--
-- doc: <http://www.csounds.com/manual/html/samphold.html>
samphold :: Sig -> Sig -> Sig
samphold = opc2 "samphold" [
    (a, [a, a, i, i]),
    (k, [k, k, i, i])]

-- | Access values of the current buffer of an a-rate variable by indexing. Useful for doing sample-by-sample manipulation at k-rate without using setksmps 1. 
--
-- > kval vaget kndx, avar
--
-- doc: <http://www.csounds.com/manual/html/vaget.html>
vaget :: Sig -> Sig -> Sig
vaget = opc2 "vaget" [(k, [k, a])]

-- | Write values into the current buffer of an a-rate variable at the given index. Useful for doing sample-by-sample manipulation at k-rate without using setksmps 1. 
--
-- > vaset kval, kndx, avar
--
-- doc: <http://www.csounds.com/manual/html/vaset.html>
vaset :: Sig -> Sig -> Sig -> SE ()
vaset a1 a2 a3 = se_ $ opc3 "vaset" [(x, [k, k, a])] a1 a2 a3

---------------------------------------------------
-- panning

-- | Distribute an audio signal amongst four channels with localization control.    
--
-- > a1, a2, a3, a4 pan asig, kx, ky, ifn [, imode] [, ioffset]
--
-- doc: <http://www.csounds.com/manual/html/pan.html>
pan :: Sig -> Sig -> Sig -> Tab -> (Sig, Sig, Sig, Sig)
pan = mopc4 "pan" ([a, a, a, a], [a, k, k, i, i, i])

-- | Distribute an audio signal across two channels with a choice of methods. 
--
-- > a1, a2 pan2 asig, xp [, imode]
--
-- doc: <http://www.csounds.com/manual/html/pan2.html>
pan2 :: Sig -> Sig -> (Sig, Sig)
pan2 = mopc2 "pan2" ([a, a], [a, x, i])


-- HRTF

-- | This opcode takes a source signal and spatialises it in the 3 dimensional space around a listener using head related 
-- transfer function (HRTF) based filters. It produces a static output (azimuth and elevation parameters are i-rate), 
-- because a static source allows much more efficient processing than hrtfmove and hrtfmove2,. 
--
-- > aleft, aright hrtfstat asrc, iAz, iElev, ifilel, ifiler [,iradius, isr]
--
-- doc: <http://www.csounds.com/manual/html/hrtfstat.html>
hrtfstat :: Sig -> D -> D -> Str -> Str -> (Sig, Sig)
hrtfstat = mopc5 "hrtfstat" ([a, a], a:s:i:i:s:is 2)

-- | This opcode takes a source signal and spatialises it in the 3 dimensional space around a listener by convolving the source with stored head related transfer function (HRTF) based filters. 
--
-- > aleft, aright hrtfmove asrc, kAz, kElev, ifilel, ifiler [, imode, ifade, isr]
--
-- doc: <http://www.csounds.com/manual/html/hrtfmove.html>
hrtfmove :: Sig -> Sig -> Sig -> Str -> Str -> (Sig, Sig)
hrtfmove = mopc5 "hrtfmove" ([a, a], a:k:k:s:s:is 3)

-- | This opcode takes a source signal and spatialises it in the 3 dimensional space around a listener using head related transfer function (HRTF) based filters. 
--
-- > aleft, aright hrtfmove2 asrc, kAz, kElev, ifilel, ifiler [,ioverlap, iradius, isr]
--
-- doc: <http://www.csounds.com/manual/html/hrtfmove2.html>
hrtfmove2 :: Sig -> Sig -> Sig -> Str -> Str -> (Sig, Sig)
hrtfmove2 = mopc5 "hrtfmove2" ([a, a], a:k:k:s:s:is 3)

--------------------------------------------------------------------------
-- Control opcodes

-- | Read absolute time, in seconds, since the start of the performance.
--
-- > ires times
-- > kres times
--
-- doc: <http://www.csounds.com/manual/html/times.html>
times :: Sig
times = opc0 "times" [(k, []), (i, [])]

-- | Read absolute time, in k-rate cycles, since the start of the performance.
--
-- > ires timek
-- > kres timek
--
-- doc: <http://www.csounds.com/manual/html/timek.html>
timek :: Sig
timek = opc0 "timek" [(k, []), (i, [])]

-- | Generate a metronomic signal to be used in any circumstance an isochronous trigger is needed.
--
-- > ktrig  metro  kfreq [, initphase]
--
-- doc: <http://www.csounds.com/manual/html/metro.html>
metro :: Cps -> Ksig 
metro = opc1 "metro" [(k, [k,i])]

-- | Behaves like 'Csound.Opcode.Basic.metro', but returns an event stream.
metroE :: Cps -> Evt ()
metroE = sigToEvt . metro


-- | Behaves like 'Csound.Opcode.Basic.changed', but returns an event stream.
changedE :: [Ksig] -> Evt ()
changedE = sigToEvt . changed

-- | Informs when a krate signal crosses a threshold.
-- 
-- > kout trigger ksig, kthreshold, kmode
--
-- doc: <http://www.csounds.com/manual/html/trigger.html>
trigger :: Sig -> Sig -> Sig -> Sig
trigger = opc3 "trigger" [(k, ks 3)]

-- | Behaves like 'Csound.Opcode.Basic.trigger', but returns an event stream.
triggerE :: Sig -> Sig -> Sig -> Evt ()
triggerE a1 a2 a3 = sigToEvt $ trigger a1 a2 a3

-- | Extend the duration of real-time generated events and handle their extra life (Usually for usage along with release instead of linenr, linsegr, etc). 
--
-- > xtratim iextradur
--
-- doc: <http://www.csounds.com/manual/html/xtratim.html>
xtratim :: D -> SE ()
xtratim a1 = se_ $ opc1 "xtratim" [(x, [i])] a1

