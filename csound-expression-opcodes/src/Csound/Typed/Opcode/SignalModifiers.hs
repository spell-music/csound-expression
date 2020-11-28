module Csound.Typed.Opcode.SignalModifiers (
    
    
    -- * Amplitude Modifiers.
    balance, clip, compress, compress2, dam, gain,
    
    -- * Convolution and Morphing.
    convolve, cross2, dconv, ftconv, ftmorf, liveconv, pconvolve, tvconv,
    
    -- * Delay.
    delay, delay1, delayk, vdel_k, delayr, delayw, deltap, deltap3, deltapi, deltapn, deltapx, deltapxw, multitap, vdelay, vdelay3, vdelayx, vdelayxq, vdelayxs, vdelayxw, vdelayxwq, vdelayxws,
    
    -- * Panning and Spatialization.
    bformdec, bformdec1, bformenc, bformenc1, hrtfearly, hrtfmove, hrtfmove2, hrtfreverb, hrtfstat, locsend, locsig, pan, pan2, space, spat3d, spat3di, spat3dt, spdist, spsend, vbap, vbap16, vbap16move, vbap4, vbap4move, vbap8, vbap8move, vbapg, vbapgmove, vbaplsinit, vbapmove, vbapz, vbapzmove,
    
    -- * Reverberation.
    alpass, babo, comb, combinv, freeverb, nestedap, nreverb, platerev, reverb, reverb2, reverbsc, valpass, vcomb,
    
    -- * Sample Level Operators.
    denorm, diff, downsamp, fold, integ, interp, ntrpol, samphold, upsamp, vaget, vaset,
    
    -- * Signal Limiters.
    limit, mirror, wrap,
    
    -- * Special Effects.
    distort, distort1, flanger, harmon, harmon2, harmon3, harmon4, phaser1, phaser2,
    
    -- * Standard Filters.
    atone, atonex, biquad, biquada, butbp, butbr, buthp, butlp, butterbp, butterbr, butterhp, butterlp, clfilt, diode_ladder, doppler, k35_hpf, k35_lpf, median, mediank, mode, tone, tonex, zdf_1pole, zdf_1pole_mode, zdf_2pole, zdf_2pole_mode, zdf_ladder,
    
    -- * Standard Filters:Resonant.
    areson, bqrez, lowpass2, lowres, lowresx, lpf18, moogladder, moogladder2, moogvcf, moogvcf2, mvchpf, mvclpf1, mvclpf2, mvclpf3, mvclpf4, reson, resonr, resonx, resony, resonz, rezzy, statevar, svfilter, tbvcf, vlowres,
    
    -- * Standard Filters:Control.
    aresonk, atonek, lineto, port, portk, resonk, resonxk, sc_lag, sc_lagud, sc_trig, tlineto, tonek,
    
    -- * Specialized Filters.
    dcblock, dcblock2, eqfil, filter2, fmanal, fofilter, hilbert, hilbert2, nlfilt, nlfilt2, pareq, rbjeq, zfilter2,
    
    -- * Waveguides.
    wguide1, wguide2,
    
    -- * Waveshaping.
    chebyshevpoly, pdclip, pdhalf, pdhalfy, powershape,
    
    -- * Comparators and Accumulators.
    cmp, max', max_k, maxabs, maxabsaccum, maxaccum, min', minabs, minabsaccum, minaccum) where

import Control.Applicative
import Control.Monad.Trans.Class
import Csound.Dynamic
import Csound.Typed

-- Amplitude Modifiers.

-- | 
-- Adjust one audio signal according to the values of another.
--
-- The rms power of asig can be interrogated, set, or adjusted to match that of a comparator signal.
--
-- > ares  balance  asig, acomp [, ihp] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/balance.html>
balance ::  Sig -> Sig -> Sig
balance b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "balance" [(Ar,[Ar,Ar,Ir,Ir])] [a1,a2]

-- | 
-- Clips a signal to a predefined limit.
--
-- Clips an a-rate signal to a predefined limit, in a âsoftâ manner, using one of three methods.
--
-- > ares  clip  asig, imeth, ilimit [, iarg]
--
-- csound doc: <http://csound.com/docs/manual/clip.html>
clip ::  Sig -> D -> D -> Sig
clip b1 b2 b3 = Sig $ f <$> unSig b1 <*> unD b2 <*> unD b3
    where f a1 a2 a3 = opcs "clip" [(Ar,[Ar,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Compress, limit, expand, duck or gate an audio signal.
--
-- This unit functions as an audio
--     compressor, limiter, expander, or noise gate, using either
--     soft-knee or hard-knee mapping, and with dynamically variable
--     performance characteristics.  It takes two audio input signals,
--     aasig and acsig, the first of which is modified by a running
--     analysis of the second. Both signals can be the same, or the first
--     can be modified by a different controlling signal.
--
-- > ar  compress  aasig, acsig, kthresh, kloknee, khiknee, kratio, katt, krel, ilook
--
-- csound doc: <http://csound.com/docs/manual/compress.html>
compress ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> Sig
compress b1 b2 b3 b4 b5 b6 b7 b8 b9 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7 <*> unSig b8 <*> unD b9
    where f a1 a2 a3 a4 a5 a6 a7 a8 a9 = opcs "compress" [(Ar,[Ar,Ar,Kr,Kr,Kr,Kr,Kr,Kr,Ir])] [a1
                                                                                             ,a2
                                                                                             ,a3
                                                                                             ,a4
                                                                                             ,a5
                                                                                             ,a6
                                                                                             ,a7
                                                                                             ,a8
                                                                                             ,a9]

-- | 
-- Compress, limit, expand, duck or gate an audio signal.
--
-- This unit functions as an audio
--     compressor, limiter, expander, or noise gate, using either
--     soft-knee or hard-knee mapping, and with dynamically variable
--     performance characteristics.  It takes two audio input signals,
--     aasig and acsig, the first of which is modified by a running
--     analysis of the second. Both signals can be the same, or the first
--     can be modified by a different controlling signal.
--
-- > ar  compress2  aasig, acsig, kthresh, kloknee, khiknee, kratio, katt, krel, ilook
--
-- csound doc: <http://csound.com/docs/manual/compress2.html>
compress2 ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> Sig
compress2 b1 b2 b3 b4 b5 b6 b7 b8 b9 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7 <*> unSig b8 <*> unD b9
    where f a1 a2 a3 a4 a5 a6 a7 a8 a9 = opcs "compress2" [(Ar,[Ar,Ar,Kr,Kr,Kr,Kr,Kr,Kr,Ir])] [a1
                                                                                              ,a2
                                                                                              ,a3
                                                                                              ,a4
                                                                                              ,a5
                                                                                              ,a6
                                                                                              ,a7
                                                                                              ,a8
                                                                                              ,a9]

-- | 
-- A dynamic compressor/expander.
--
-- This opcode dynamically modifies a gain value applied to the input sound ain by comparing its power level to a given threshold level. The signal will be compressed/expanded with different factors regarding that it is over or under the threshold.
--
-- > ares  dam  asig, kthreshold, icomp1, icomp2, irtime, iftime
--
-- csound doc: <http://csound.com/docs/manual/dam.html>
dam ::  Sig -> Sig -> D -> D -> D -> D -> Sig
dam b1 b2 b3 b4 b5 b6 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unD b6
    where f a1 a2 a3 a4 a5 a6 = opcs "dam" [(Ar,[Ar,Kr,Ir,Ir,Ir,Ir])] [a1,a2,a3,a4,a5,a6]

-- | 
-- Adjusts the amplitude audio signal according to a root-mean-square value.
--
-- > ares  gain  asig, krms [, ihp] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/gain.html>
gain ::  Sig -> Sig -> Sig
gain b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "gain" [(Ar,[Ar,Kr,Ir,Ir])] [a1,a2]

-- Convolution and Morphing.

-- | 
-- Convolves a signal and an impulse response.
--
-- Output is the convolution of signal ain and the impulse response contained in ifilcod. If more than one output signal is supplied, each will be convolved with the same impulse response. Note that it is considerably more efficient to use one instance of the operator when processing a mono input to create stereo, or quad, outputs.
--
-- > ar1 [, ar2] [, ar3] [, ar4]  convolve  ain, ifilcod [, ichannel]
--
-- csound doc: <http://csound.com/docs/manual/convolve.html>
convolve :: Tuple a => Sig -> Str -> a
convolve b1 b2 = pureTuple $ f <$> unSig b1 <*> unStr b2
    where f a1 a2 = mopcs "convolve" ([Ar,Ar,Ar,Ar],[Ar,Sr,Ir]) [a1,a2]

-- | 
-- Cross synthesis using FFT's.
--
-- This is an implementation of cross synthesis using FFT's.
--
-- > ares  cross2  ain1, ain2, isize, ioverlap, iwin, kbias
--
-- csound doc: <http://csound.com/docs/manual/cross2.html>
cross2 ::  Sig -> Sig -> D -> D -> D -> Sig -> Sig
cross2 b1 b2 b3 b4 b5 b6 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unSig b6
    where f a1 a2 a3 a4 a5 a6 = opcs "cross2" [(Ar,[Ar,Ar,Ir,Ir,Ir,Kr])] [a1,a2,a3,a4,a5,a6]

-- | 
-- A direct convolution opcode.
--
-- > ares  dconv  asig, isize, ifn
--
-- csound doc: <http://csound.com/docs/manual/dconv.html>
dconv ::  Sig -> D -> Tab -> Sig
dconv b1 b2 b3 = Sig $ f <$> unSig b1 <*> unD b2 <*> unTab b3
    where f a1 a2 a3 = opcs "dconv" [(Ar,[Ar,Ir,Ir])] [a1,a2,a3]

-- | 
-- Low latency multichannel convolution, using a function table as impulse
-- 	response source.
--
-- Low latency multichannel convolution, using a function table as impulse
-- 	response source. The algorithm is to split the impulse response to
-- 	partitions of length determined by the iplen parameter, and delay and
-- 	mix partitions so that the original, full length impulse response is
-- 	reconstructed without gaps. The output delay (latency) is iplen samples,
-- 	and does not depend on the control rate, unlike in the case of other
-- 	convolve opcodes.
--
-- > a1[, a2[, a3[, ... a8]]]  ftconv  ain, ift, iplen[, iskipsamples \
-- >           [, iirlen[, iskipinit]]]
--
-- csound doc: <http://csound.com/docs/manual/ftconv.html>
ftconv :: Tuple a => Sig -> D -> D -> a
ftconv b1 b2 b3 = pureTuple $ f <$> unSig b1 <*> unD b2 <*> unD b3
    where f a1 a2 a3 = mopcs "ftconv" ((repeat Ar),[Ar,Ir,Ir,Ir,Ir,Ir]) [a1,a2,a3]

-- | 
-- Morphs between multiple ftables as specified in a list.
--
-- Uses an index into a table of ftable numbers to morph between adjacent tables in the list.This morphed function is written into the table referenced by iresfn on every k-cycle.
--
-- >  ftmorf  kftndx, iftfn, iresfn
--
-- csound doc: <http://csound.com/docs/manual/ftmorf.html>
ftmorf ::  Sig -> Tab -> Tab -> SE ()
ftmorf b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unTab b2 <*> unTab b3
    where f a1 a2 a3 = opcs "ftmorf" [(Xr,[Kr,Ir,Ir])] [a1,a2,a3]

-- | 
-- Partitioned convolution with dynamically reloadable impulse response
--
-- Computationally efficient, partitioned convolution, using a function table as impulse response (IR) source,
--       similar to the ftconv opcode. 
--       The liveconv opcode allows dynamic reload of IR data at any time
--       while the convolution is running, controlled by the kupdate parameter.
--       Due to the manner in which the IR is updated, the operation can be done without audio artifacts in the convolution output.
--
-- > ares  liveconv  ain, ift, iplen, kupdate, kclear
--
-- csound doc: <http://csound.com/docs/manual/liveconv.html>
liveconv ::  Sig -> D -> D -> Sig -> Sig -> Sig
liveconv b1 b2 b3 b4 b5 = Sig $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unSig b4 <*> unSig b5
    where f a1 a2 a3 a4 a5 = opcs "liveconv" [(Ar,[Ar,Ir,Ir,Kr,Kr])] [a1,a2,a3,a4,a5]

-- | 
-- Convolution based on a uniformly partitioned overlap-save algorithm
--
-- Convolution based on a uniformly partitioned overlap-save algorithm. Compared to the convolve opcode, pconvolve has these benefits:
--
-- > ar1 [, ar2] [, ar3] [, ar4]  pconvolve  ain, ifilcod [, ipartitionsize, ichannel]
--
-- csound doc: <http://csound.com/docs/manual/pconvolve.html>
pconvolve :: Tuple a => Sig -> Str -> a
pconvolve b1 b2 = pureTuple $ f <$> unSig b1 <*> unStr b2
    where f a1 a2 = mopcs "pconvolve" ([Ar,Ar,Ar,Ar],[Ar,Sr,Ir,Ir]) [a1,a2]

-- | 
-- A time-varying convolution (FIR filter) opcode.
--
-- An opcode that takes two incoming signals and
--       interprets one of them as the coefficients of linear
--       time-variable finite impulse response filter. This is
--       implemented via direct convolution (for partition sizes of
--       1 sample) or DFT-based partitioned convolution.
--       The signals can be 'frozen' (i.e. the filter coefficients are
--       kept the same) at any point in time, at a-rate or k-rate.
--
-- > ares  tvconv  asig1, asig2, xfreez1,
-- >         xfreez2, iparts, ifils
--
-- csound doc: <http://csound.com/docs/manual/tvconv.html>
tvconv ::  Sig -> Sig -> Sig -> Sig -> D -> D -> Sig
tvconv b1 b2 b3 b4 b5 b6 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unD b5 <*> unD b6
    where f a1 a2 a3 a4 a5 a6 = opcs "tvconv" [(Ar,[Ar,Ar,Xr,Xr,Ir,Ir])] [a1,a2,a3,a4,a5,a6]

-- Delay.

-- | 
-- Delays an input signal by some time interval.
--
-- A signal can be read from or written into a delay path, or it can be automatically delayed by some time interval.
--
-- > ares  delay  asig, idlt [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/delay.html>
delay ::  Sig -> D -> Sig
delay b1 b2 = Sig $ f <$> unSig b1 <*> unD b2
    where f a1 a2 = opcs "delay" [(Ar,[Ar,Ir,Ir])] [a1,a2]

-- | 
-- Delays an input signal by one sample.
--
-- > ares  delay1  asig [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/delay1.html>
delay1 ::  Sig -> Sig
delay1 b1 = Sig $ f <$> unSig b1
    where f a1 = opcs "delay1" [(Ar,[Ar,Ir])] [a1]

-- | 
-- Delays an input signal by some time interval.
--
-- k-rate delay opcodes
--
-- > kr  delayk   ksig, idel[, imode]
--
-- csound doc: <http://csound.com/docs/manual/delayk.html>
delayk ::  Sig -> D -> Sig
delayk b1 b2 = Sig $ f <$> unSig b1 <*> unD b2
    where f a1 a2 = opcs "delayk" [(Kr,[Kr,Ir,Ir])] [a1,a2]

-- | 
-- Delays an input signal by some time interval.
--
-- k-rate delay opcodes
--
-- > kr  vdel_k   ksig, kdel, imdel[, imode]
--
-- csound doc: <http://csound.com/docs/manual/delayk.html>
vdel_k ::  Sig -> Sig -> D -> Sig
vdel_k b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3
    where f a1 a2 a3 = opcs "vdel_k" [(Kr,[Kr,Kr,Ir,Ir])] [a1,a2,a3]

-- | 
-- Reads from an automatically established digital delay line.
--
-- > ares  delayr  idlt [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/delayr.html>
delayr ::  D -> SE Sig
delayr b1 = fmap ( Sig . return) $ SE $ (depT =<<) $ lift $ f <$> unD b1
    where f a1 = opcs "delayr" [(Ar,[Ir,Ir])] [a1]

-- | 
-- Writes the audio signal to a digital delay line.
--
-- >  delayw  asig
--
-- csound doc: <http://csound.com/docs/manual/delayw.html>
delayw ::  Sig -> SE ()
delayw b1 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1
    where f a1 = opcs "delayw" [(Xr,[Ar])] [a1]

-- | 
-- Taps a delay line at variable offset times.
--
-- Tap a delay line at variable offset times.
--
-- > ares  deltap  kdlt
--
-- csound doc: <http://csound.com/docs/manual/deltap.html>
deltap ::  Sig -> SE Sig
deltap b1 = fmap ( Sig . return) $ SE $ (depT =<<) $ lift $ f <$> unSig b1
    where f a1 = opcs "deltap" [(Ar,[Kr])] [a1]

-- | 
-- Taps a delay line at variable offset times, uses cubic interpolation.
--
-- > ares  deltap3  xdlt
--
-- csound doc: <http://csound.com/docs/manual/deltap3.html>
deltap3 ::  Sig -> SE Sig
deltap3 b1 = fmap ( Sig . return) $ SE $ (depT =<<) $ lift $ f <$> unSig b1
    where f a1 = opcs "deltap3" [(Ar,[Xr])] [a1]

-- | 
-- Taps a delay line at variable offset times, uses interpolation.
--
-- > ares  deltapi  xdlt
--
-- csound doc: <http://csound.com/docs/manual/deltapi.html>
deltapi ::  Sig -> SE Sig
deltapi b1 = fmap ( Sig . return) $ SE $ (depT =<<) $ lift $ f <$> unSig b1
    where f a1 = opcs "deltapi" [(Ar,[Xr])] [a1]

-- | 
-- Taps a delay line at variable offset times.
--
-- Tap a delay line at variable offset times.
--
-- > ares  deltapn  xnumsamps
--
-- csound doc: <http://csound.com/docs/manual/deltapn.html>
deltapn ::  Sig -> Sig
deltapn b1 = Sig $ f <$> unSig b1
    where f a1 = opcs "deltapn" [(Ar,[Xr])] [a1]

-- | 
-- Read from or write to a delay line with interpolation.
--
-- deltapx is similar to deltapi or deltap3. However, it allows higher quality interpolation. This opcode can read from and write to a delayr/delayw delay line with interpolation.
--
-- > aout  deltapx  adel, iwsize
--
-- csound doc: <http://csound.com/docs/manual/deltapx.html>
deltapx ::  Sig -> D -> SE Sig
deltapx b1 b2 = fmap ( Sig . return) $ SE $ (depT =<<) $ lift $ f <$> unSig b1 <*> unD b2
    where f a1 a2 = opcs "deltapx" [(Ar,[Ar,Ir])] [a1,a2]

-- | 
-- Mixes the input signal to a delay line.
--
-- deltapxw mixes the input signal to a delay line. This opcode can be mixed with reading units (deltap, deltapn, deltapi, deltap3, and deltapx) in any order; the actual delay time is the difference of the read and write time. This opcode can read from and write to a delayr/delayw delay line with interpolation.
--
-- >  deltapxw  ain, adel, iwsize
--
-- csound doc: <http://csound.com/docs/manual/deltapxw.html>
deltapxw ::  Sig -> Sig -> D -> SE ()
deltapxw b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unSig b2 <*> unD b3
    where f a1 a2 a3 = opcs "deltapxw" [(Xr,[Ar,Ar,Ir])] [a1,a2,a3]

-- | 
-- Multitap delay line implementation.
--
-- > ares  multitap  asig [, itime1, igain1] [, itime2, igain2] [...]
--
-- csound doc: <http://csound.com/docs/manual/multitap.html>
multitap ::  Sig -> [D] -> Sig
multitap b1 b2 = Sig $ f <$> unSig b1 <*> mapM unD b2
    where f a1 a2 = opcs "multitap" [(Ar,[Ar] ++ (repeat Ir))] ([a1] ++ a2)

-- | 
-- An interpolating variable time delay.
--
-- This is an interpolating variable time delay, it is not very different from the existing implementation (deltapi), it is only easier to use.
--
-- > ares  vdelay  asig, adel, imaxdel [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/vdelay.html>
vdelay ::  Sig -> Sig -> D -> Sig
vdelay b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3
    where f a1 a2 a3 = opcs "vdelay" [(Ar,[Ar,Ar,Ir,Ir])] [a1,a2,a3]

-- | 
-- A variable time delay with cubic interpolation.
--
-- vdelay3 is experimental. It is the same as vdelay except that it uses cubic interpolation. (New in Version 3.50.)
--
-- > ares  vdelay3  asig, adel, imaxdel [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/vdelay3.html>
vdelay3 ::  Sig -> Sig -> D -> Sig
vdelay3 b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3
    where f a1 a2 a3 = opcs "vdelay3" [(Ar,[Ar,Ar,Ir,Ir])] [a1,a2,a3]

-- | 
-- A variable delay opcode with high quality interpolation.
--
-- > aout  vdelayx  ain, adl, imd, iws [, ist]
--
-- csound doc: <http://csound.com/docs/manual/vdelayx.html>
vdelayx ::  Sig -> Sig -> D -> D -> Sig
vdelayx b1 b2 b3 b4 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unD b4
    where f a1 a2 a3 a4 = opcs "vdelayx" [(Ar,[Ar,Ar,Ir,Ir,Ir])] [a1,a2,a3,a4]

-- | 
-- A 4-channel variable delay opcode with high quality interpolation.
--
-- > aout1, aout2, aout3, aout4  vdelayxq  ain1, ain2, ain3, ain4, adl, imd, iws [, ist]
--
-- csound doc: <http://csound.com/docs/manual/vdelayxq.html>
vdelayxq ::  Sig -> Sig -> Sig -> Sig -> Sig -> D -> D -> (Sig,Sig,Sig,Sig)
vdelayxq b1 b2 b3 b4 b5 b6 b7 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unD b6 <*> unD b7
    where f a1 a2 a3 a4 a5 a6 a7 = mopcs "vdelayxq" ([Ar,Ar,Ar,Ar],[Ar,Ar,Ar,Ar,Ar,Ir,Ir,Ir]) [a1
                                                                                              ,a2
                                                                                              ,a3
                                                                                              ,a4
                                                                                              ,a5
                                                                                              ,a6
                                                                                              ,a7]

-- | 
-- A stereo variable delay opcode with high quality interpolation.
--
-- > aout1, aout2  vdelayxs  ain1, ain2, adl, imd, iws [, ist]
--
-- csound doc: <http://csound.com/docs/manual/vdelayxs.html>
vdelayxs ::  Sig -> Sig -> Sig -> D -> D -> (Sig,Sig)
vdelayxs b1 b2 b3 b4 b5 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4 <*> unD b5
    where f a1 a2 a3 a4 a5 = mopcs "vdelayxs" ([Ar,Ar],[Ar,Ar,Ar,Ir,Ir,Ir]) [a1,a2,a3,a4,a5]

-- | 
-- Variable delay opcodes with high quality interpolation.
--
-- > aout  vdelayxw  ain, adl, imd, iws [, ist]
--
-- csound doc: <http://csound.com/docs/manual/vdelayxw.html>
vdelayxw ::  Sig -> Sig -> D -> D -> Sig
vdelayxw b1 b2 b3 b4 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unD b4
    where f a1 a2 a3 a4 = opcs "vdelayxw" [(Ar,[Ar,Ar,Ir,Ir,Ir])] [a1,a2,a3,a4]

-- | 
-- Variable delay opcodes with high quality interpolation.
--
-- > aout1, aout2, aout3, aout4  vdelayxwq  ain1, ain2, ain3, ain4, adl, \
-- >           imd, iws [, ist]
--
-- csound doc: <http://csound.com/docs/manual/vdelayxwq.html>
vdelayxwq ::  Sig -> Sig -> Sig -> Sig -> Sig -> D -> D -> (Sig,Sig,Sig,Sig)
vdelayxwq b1 b2 b3 b4 b5 b6 b7 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unD b6 <*> unD b7
    where f a1 a2 a3 a4 a5 a6 a7 = mopcs "vdelayxwq" ([Ar,Ar,Ar,Ar],[Ar,Ar,Ar,Ar,Ar,Ir,Ir,Ir]) [a1
                                                                                               ,a2
                                                                                               ,a3
                                                                                               ,a4
                                                                                               ,a5
                                                                                               ,a6
                                                                                               ,a7]

-- | 
-- Variable delay opcodes with high quality interpolation.
--
-- > aout1, aout2  vdelayxws  ain1, ain2, adl, imd, iws [, ist]
--
-- csound doc: <http://csound.com/docs/manual/vdelayxws.html>
vdelayxws ::  Sig -> Sig -> Sig -> D -> D -> (Sig,Sig)
vdelayxws b1 b2 b3 b4 b5 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4 <*> unD b5
    where f a1 a2 a3 a4 a5 = mopcs "vdelayxws" ([Ar,Ar],[Ar,Ar,Ar,Ir,Ir,Ir]) [a1,a2,a3,a4,a5]

-- Panning and Spatialization.

-- | 
-- Deprecated. Decodes an ambisonic B format signal.
--
-- Decodes an ambisonic B format signal into loudspeaker specific signals. Note that this opcode is
--       deprecated as it is inaccurate, and is replaced by the much
--       better opcode bformdec1 which replicates all
--       the important features.
--
-- > ao1, ao2  bformdec  isetup, aw, ax, ay, az [, ar, as, at, au, av \
-- >           [, abk, al, am, an, ao, ap, aq]]
-- > ao1, ao2, ao3, ao4  bformdec  isetup, aw, ax, ay, az [, ar, as, at, \
-- >           au, av [, abk, al, am, an, ao, ap, aq]]
-- > ao1, ao2, ao3, ao4, ao5  bformdec  isetup, aw, ax, ay, az [, ar, as, \
-- >           at, au, av [, abk, al, am, an, ao, ap, aq]]
-- > ao1, ao2, ao3, ao4, ao5, ao6, ao7, ao8  bformdec  isetup, aw, ax, ay, az \
-- >           [, ar, as, at, au, av [, abk, al, am, an, ao, ap, aq]]]
--
-- csound doc: <http://csound.com/docs/manual/bformdec.html>
bformdec :: Tuple a => D -> Sig -> Sig -> Sig -> Sig -> a
bformdec b1 b2 b3 b4 b5 = pureTuple $ f <$> unD b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5
    where f a1 a2 a3 a4 a5 = mopcs "bformdec" ([Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
                                              ,[Ir,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]) [a1,a2,a3,a4,a5]

-- | 
-- Decodes an ambisonic B format signal
--
-- Decodes an ambisonic B format signal into loudspeaker specific signals.
--
-- > ao1, ao2  bformdec1  isetup, aw, ax, ay, az [, ar, as, at, au, av \
-- >           [, abk, al, am, an, ao, ap, aq]]
-- > ao1, ao2, ao3, ao4  bformdec1  isetup, aw, ax, ay, az [, ar, as, at, \
-- >           au, av [, abk, al, am, an, ao, ap, aq]]
-- > ao1, ao2, ao3, ao4, ao5  bformdec1  isetup, aw, ax, ay, az [, ar, as, \
-- >           at, au, av [, abk, al, am, an, ao, ap, aq]]
-- > ao1, ao2, ao3, ao4, ao5, ao6, ao7, ao8  bformdec1  isetup, aw, ax, ay, az \
-- >           [, ar, as, at, au, av [, abk, al, am, an, ao, ap,
-- >         aq]]]
-- > aout[]  bformdec1  isetup, abform[]
--
-- csound doc: <http://csound.com/docs/manual/bformdec1.html>
bformdec1 :: Tuple a => D -> Sig -> Sig -> Sig -> Sig -> a
bformdec1 b1 b2 b3 b4 b5 = pureTuple $ f <$> unD b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5
    where f a1 a2 a3 a4 a5 = mopcs "bformdec1" ([Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
                                               ,[Ir,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]) [a1,a2,a3,a4,a5]

-- | 
-- Deprecated. Codes a signal into the ambisonic B format.
--
-- Codes a signal into the ambisonic B format. Note that this opcode is
--       deprecated as it is inaccurate, and is replaced by the much
--       better
--       opcode bformenc1
--       which replicates all the important features; also note that the
--       gain arguments are not available in bformenc1.
--
-- > aw, ax, ay, az  bformenc  asig, kalpha, kbeta, kord0, kord1
-- > aw, ax, ay, az, ar, as, at, au, av  bformenc  asig, kalpha, kbeta, \
-- >           kord0, kord1 , kord2
-- > aw, ax, ay, az, ar, as, at, au, av, ak, al, am, an, ao, ap, aq  bformenc  \
-- >           asig, kalpha, kbeta, kord0, kord1, kord2, kord3
--
-- csound doc: <http://csound.com/docs/manual/bformenc.html>
bformenc :: Tuple a => Sig -> Sig -> Sig -> Sig -> Sig -> a
bformenc b1 b2 b3 b4 b5 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5
    where f a1 a2 a3 a4 a5 = mopcs "bformenc" ([Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
                                              ,[Ar,Kr,Kr,Kr,Kr,Kr,Kr]) [a1,a2,a3,a4,a5]

-- | 
-- Codes a signal into the ambisonic B format.
--
-- Codes a signal into the ambisonic B format
--
-- > aw, ax, ay, az  bformenc1  asig, kalpha, kbeta
-- > aw, ax, ay, az, ar, as, at, au, av  bformenc1  asig, kalpha, kbeta
-- > aw, ax, ay, az, ar, as, at, au, av, ak, al, am, an, ao, ap, aq  bformenc1  \
-- >           asig, kalpha, kbeta
-- > aarray[]  bformenc1  asig, kalpha, kbeta
--
-- csound doc: <http://csound.com/docs/manual/bformenc1.html>
bformenc1 :: Tuple a => Sig -> Sig -> Sig -> a
bformenc1 b1 b2 b3 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = mopcs "bformenc1" ([Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
                                         ,[Ar,Kr,Kr]) [a1,a2,a3]

-- | 
-- Generates 3D binaural audio with high-fidelity early reflections in a parametric room using a Phase Truncation algorithm.
--
-- This opcode essentially nests the hrtfmove opcode in an image model for a user-definable shoebox-shaped room. A default room can be selected, or advanced room parameters can be used. Room surfaces can be controlled with high and low-frequency absorption coefficients and gain factors of a three-band equaliser.
--
-- > aleft, aright, irt60low, irt60high, imfp  hrtfearly  asrc, ksrcx, ksrcy, ksrcz, klstnrx, klstnry, klstnrz, \
-- >           ifilel, ifiler, idefroom [,ifade, isr, iorder, ithreed, kheadrot, iroomx, iroomy, iroomz, iwallhigh, \
-- >           iwalllow, iwallgain1, iwallgain2, iwallgain3, ifloorhigh, ifloorlow, ifloorgain1, ifloorgain2, \
-- >           ifloorgain3, iceilinghigh, iceilinglow, iceilinggain1, iceilinggain2, iceilinggain3]
--
-- csound doc: <http://csound.com/docs/manual/hrtfearly.html>
hrtfearly :: Tuple a => Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> D -> D -> a
hrtfearly b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7 <*> unD b8 <*> unD b9 <*> unD b10
    where f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = mopcs "hrtfearly" ([Ar,Ar,Ir,Ir,Ir]
                                                               ,[Ar
                                                                ,Kr
                                                                ,Kr
                                                                ,Kr
                                                                ,Kr
                                                                ,Kr
                                                                ,Kr
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Kr
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir
                                                                ,Ir]) [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10]

-- | 
-- Generates dynamic 3d binaural audio for headphones using magnitude interpolation and phase truncation.
--
-- This opcode takes a source signal and spatialises it in the 3 dimensional space around a listener
--       by convolving the source with stored head related transfer function (HRTF) based filters.
--
-- > aleft, aright  hrtfmove  asrc, kAz, kElev, ifilel, ifiler [, imode, ifade, isr]
--
-- csound doc: <http://csound.com/docs/manual/hrtfmove.html>
hrtfmove ::  Sig -> Sig -> Sig -> D -> D -> (Sig,Sig)
hrtfmove b1 b2 b3 b4 b5 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4 <*> unD b5
    where f a1 a2 a3 a4 a5 = mopcs "hrtfmove" ([Ar,Ar],[Ar,Kr,Kr,Ir,Ir,Ir,Ir,Ir]) [a1,a2,a3,a4,a5]

-- | 
-- Generates dynamic 3d binaural audio for headphones using a Woodworth based spherical head model
--       with improved low frequency phase accuracy.
--
-- This opcode takes a source signal and spatialises it in the 3 dimensional space around a listener
--       using head related transfer function (HRTF) based filters.
--
-- > aleft, aright  hrtfmove2  asrc, kAz, kElev, ifilel, ifiler [,ioverlap, iradius, isr]
--
-- csound doc: <http://csound.com/docs/manual/hrtfmove2.html>
hrtfmove2 ::  Sig -> Sig -> Sig -> D -> D -> (Sig,Sig)
hrtfmove2 b1 b2 b3 b4 b5 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4 <*> unD b5
    where f a1 a2 a3 a4 a5 = mopcs "hrtfmove2" ([Ar,Ar],[Ar,Kr,Kr,Ir,Ir,Ir,Ir,Ir]) [a1,a2,a3,a4,a5]

-- | 
-- A binaural, dynamic FDN based diffuse-field reverberator. The opcode works independently as an efficient, flexible reverberator.
--
-- A frequency-dependent, efficient reverberant field is created based on low and high frequency desired reverb times. The opcode is designed to work with hrtfearly, ideally using its outputs as inputs. However, hrtfreverb can be used as a standalone tool. Stability is enforced.
--
-- > aleft, aright, idel  hrtfreverb  asrc, ilowrt60, ihighrt60, ifilel, ifiler [,isr, imfp, iorder]
--
-- csound doc: <http://csound.com/docs/manual/hrtfreverb.html>
hrtfreverb ::  Sig -> D -> D -> D -> D -> (Sig,Sig,D)
hrtfreverb b1 b2 b3 b4 b5 = pureTuple $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5
    where f a1 a2 a3 a4 a5 = mopcs "hrtfreverb" ([Ar,Ar,Ir],[Ar,Ir,Ir,Ir,Ir,Ir,Ir,Ir]) [a1
                                                                                       ,a2
                                                                                       ,a3
                                                                                       ,a4
                                                                                       ,a5]

-- | 
-- Generates static 3d binaural audio for headphones using a
--       Woodworth based spherical head model with improved low frequency
--       phase accuracy.
--
-- This opcode takes a source signal and spatialises it in the 3 dimensional space around a listener using head related transfer function (HRTF) based filters. It produces a static output (azimuth and elevation parameters are i-rate), because a static source allows much more efficient processing than hrtfmove and hrtfmove2,.
--
-- >       aleft, aright  hrtfstat  asrc, iAz, iElev, ifilel, ifiler [,iradius, isr]
-- >         
--
-- csound doc: <http://csound.com/docs/manual/hrtfstat.html>
hrtfstat ::  Sig -> D -> D -> D -> D -> (Sig,Sig)
hrtfstat b1 b2 b3 b4 b5 = pureTuple $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5
    where f a1 a2 a3 a4 a5 = mopcs "hrtfstat" ([Ar,Ar],[Ar,Ir,Ir,Ir,Ir,Ir,Ir]) [a1,a2,a3,a4,a5]

-- | 
-- Distributes the audio signals of a previous locsig opcode.
--
-- locsend depends upon the existence of a previously defined locsig. The number of output signals must match the number in the previous locsig. The output signals from locsend are derived from the values given for distance and reverb in the locsig and are ready to be sent to local or global reverb units (see example below). The reverb amount and the balance between the 2 or 4 channels are calculated in the same way as described in the Dodge book (an essential text!).
--
-- > a1, a2  locsend 
-- > a1, a2,  a3, a4  locsend 
--
-- csound doc: <http://csound.com/docs/manual/locsend.html>
locsend ::   (Sig,Sig,Sig,Sig)
locsend  = pureTuple $ return $ f 
    where f  = mopcs "locsend" ([Ar,Ar,Ar,Ar],[]) []

-- | 
-- Takes an input signal and distributes between 2 or 4 channels.
--
-- locsig takes an input signal and distributes it among 2 or 4 channels using values in degrees to calculate the balance between adjacent channels. It also takes arguments for distance (used to attenuate signals that are to sound as if they are some distance further than the loudspeaker itself), and for the amount the signal that will be sent to reverberators. This unit is based upon the example in the Charles Dodge/Thomas Jerse book, Computer Music, page 320.
--
-- > a1, a2  locsig  asig, kdegree, kdistance, kreverbsend
-- > a1, a2,  a3, a4  locsig  asig, kdegree, kdistance, kreverbsend
--
-- csound doc: <http://csound.com/docs/manual/locsig.html>
locsig ::  Sig -> Sig -> Sig -> Sig -> (Sig,Sig,Sig,Sig)
locsig b1 b2 b3 b4 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
    where f a1 a2 a3 a4 = mopcs "locsig" ([Ar,Ar,Ar,Ar],[Ar,Kr,Kr,Kr]) [a1,a2,a3,a4]

-- | 
-- Distribute an audio signal amongst four channels.
--
-- Distribute an audio signal amongst four channels with localization control.
--
-- > a1, a2, a3, a4  pan  asig, kx, ky, ifn [, imode] [, ioffset]
--
-- csound doc: <http://csound.com/docs/manual/pan.html>
pan ::  Sig -> Sig -> Sig -> Tab -> (Sig,Sig,Sig,Sig)
pan b1 b2 b3 b4 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unTab b4
    where f a1 a2 a3 a4 = mopcs "pan" ([Ar,Ar,Ar,Ar],[Ar,Kr,Kr,Ir,Ir,Ir]) [a1,a2,a3,a4]

-- | 
-- Distribute an audio signal across two channels.
--
-- Distribute an audio signal across two channels with a choice of methods.
--
-- > a1, a2  pan2  asig, xp [, imode]
--
-- csound doc: <http://csound.com/docs/manual/pan2.html>
pan2 ::  Sig -> Sig -> (Sig,Sig)
pan2 b1 b2 = pureTuple $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = mopcs "pan2" ([Ar,Ar],[Ar,Xr,Ir]) [a1,a2]

-- | 
-- Distributes an input signal among 4 channels using cartesian coordinates.
--
-- space takes an input signal and distributes it among 4 channels using Cartesian xy coordinates to calculate the balance of the outputs. The xy coordinates can be defined in a separate text file and accessed through a Function statement in the score using Gen28, or they can be specified using the optional kx, ky arguments. The advantages to the former are:
--
-- > a1, a2, a3, a4   space  asig, ifn, ktime, kreverbsend, kx, ky
--
-- csound doc: <http://csound.com/docs/manual/space.html>
space ::  Sig -> Tab -> Sig -> Sig -> Sig -> Sig -> (Sig,Sig,Sig,Sig)
space b1 b2 b3 b4 b5 b6 = pureTuple $ f <$> unSig b1 <*> unTab b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6
    where f a1 a2 a3 a4 a5 a6 = mopcs "space" ([Ar,Ar,Ar,Ar],[Ar,Ir,Kr,Kr,Kr,Kr]) [a1
                                                                                  ,a2
                                                                                  ,a3
                                                                                  ,a4
                                                                                  ,a5
                                                                                  ,a6]

-- | 
-- Positions the input sound in a 3D space and allows moving the sound at k-rate.
--
-- This opcode positions the input sound in a 3D space, with optional simulation of room acoustics, in various output formats. spat3d allows moving the sound at k-rate (this movement is interpolated internally to eliminate "zipper noise" if sr not equal to kr).
--
-- > aW, aX, aY, aZ  spat3d  ain, kX, kY, kZ, idist, ift, imode, imdel, iovr [, istor]
--
-- csound doc: <http://csound.com/docs/manual/spat3d.html>
spat3d ::  Sig -> Sig -> Sig -> Sig -> D -> D -> D -> D -> D -> (Sig,Sig,Sig,Sig)
spat3d b1 b2 b3 b4 b5 b6 b7 b8 b9 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unD b5 <*> unD b6 <*> unD b7 <*> unD b8 <*> unD b9
    where f a1 a2 a3 a4 a5 a6 a7 a8 a9 = mopcs "spat3d" ([Ar,Ar,Ar,Ar]
                                                        ,[Ar,Kr,Kr,Kr,Ir,Ir,Ir,Ir,Ir,Ir]) [a1,a2,a3,a4,a5,a6,a7,a8,a9]

-- | 
-- Positions the input sound in a 3D space with the sound source position set at i-time.
--
-- This opcode positions the input sound in a 3D space, with optional simulation of room acoustics, in various output formats. With spat3di, sound source position is set at i-time.
--
-- > aW, aX, aY, aZ  spat3di  ain, iX, iY, iZ, idist, ift, imode [, istor]
--
-- csound doc: <http://csound.com/docs/manual/spat3di.html>
spat3di ::  Sig -> D -> D -> D -> D -> D -> D -> (Sig,Sig,Sig,Sig)
spat3di b1 b2 b3 b4 b5 b6 b7 = pureTuple $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unD b6 <*> unD b7
    where f a1 a2 a3 a4 a5 a6 a7 = mopcs "spat3di" ([Ar,Ar,Ar,Ar],[Ar,Ir,Ir,Ir,Ir,Ir,Ir,Ir]) [a1
                                                                                             ,a2
                                                                                             ,a3
                                                                                             ,a4
                                                                                             ,a5
                                                                                             ,a6
                                                                                             ,a7]

-- | 
-- Can be used to render an impulse response for a 3D space at i-time.
--
-- This opcode positions the input sound in a 3D space, with optional simulation of room acoustics, in various output formats. spat3dt can be used to render the impulse response at i-time, storing output in a function table, suitable for convolution.
--
-- >  spat3dt  ioutft, iX, iY, iZ, idist, ift, imode, irlen [, iftnocl]
--
-- csound doc: <http://csound.com/docs/manual/spat3dt.html>
spat3dt ::  D -> D -> D -> D -> D -> D -> D -> D -> SE ()
spat3dt b1 b2 b3 b4 b5 b6 b7 b8 = SE $ (depT_ =<<) $ lift $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unD b6 <*> unD b7 <*> unD b8
    where f a1 a2 a3 a4 a5 a6 a7 a8 = opcs "spat3dt" [(Xr,[Ir,Ir,Ir,Ir,Ir,Ir,Ir,Ir,Ir])] [a1
                                                                                         ,a2
                                                                                         ,a3
                                                                                         ,a4
                                                                                         ,a5
                                                                                         ,a6
                                                                                         ,a7
                                                                                         ,a8]

-- | 
-- Calculates distance values from xy coordinates.
--
-- spdist uses the same xy data as space, also either from a text file using Gen28 or from x and y arguments given to the unit directly. The purpose of this unit is to make available the values for distance that are calculated from the xy coordinates.
--
-- > k1  spdist  ifn, ktime, kx, ky
--
-- csound doc: <http://csound.com/docs/manual/spdist.html>
spdist ::  Tab -> Sig -> Sig -> Sig -> Sig
spdist b1 b2 b3 b4 = Sig $ f <$> unTab b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
    where f a1 a2 a3 a4 = opcs "spdist" [(Kr,[Ir,Kr,Kr,Kr])] [a1,a2,a3,a4]

-- | 
-- Generates output signals based on a previously defined space opcode.
--
-- spsend depends upon the existence of a previously defined space. The output signals from spsend are derived from the values given for xy and reverb in the space and are ready to be sent to local or global reverb units (see example below).
--
-- > a1, a2, a3, a4  spsend 
--
-- csound doc: <http://csound.com/docs/manual/spsend.html>
spsend ::   (Sig,Sig,Sig,Sig)
spsend  = pureTuple $ return $ f 
    where f  = mopcs "spsend" ([Ar,Ar,Ar,Ar],[]) []

-- | 
-- Distributes an audio signal among many channels.
--
-- Distributes an audio signal amongmany channels, up to 64 in the
--       first form, arbitrary in the second.
--
-- > ar1[, ar2...]  vbap  asig, kazim [,
-- >         kelev] [, kspread] [, ilayout]
-- > array[]  vbap  asig, kazim [,
-- >         kelev] [, kspread] [, ilayout]
--
-- csound doc: <http://csound.com/docs/manual/vbap.html>
vbap :: Tuple a => Sig -> Sig -> a
vbap b1 b2 = pureTuple $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = mopcs "vbap" ((repeat Ar),[Ar,Kr,Kr,Kr,Ir]) [a1,a2]

-- | 
-- Distributes an audio signal among 16 channels.
--
-- > ar1, ..., ar16  vbap16  asig, kazim [, kelev] [, kspread]
--
-- csound doc: <http://csound.com/docs/manual/vbap16.html>
vbap16 :: Tuple a => Sig -> Sig -> a
vbap16 b1 b2 = pureTuple $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = mopcs "vbap16" ((repeat Ar),[Ar,Kr,Kr,Kr]) [a1,a2]

-- | 
-- Distribute an audio signal among 16 channels with moving virtual sources.
--
-- > ar1, ..., ar16  vbap16move  asig, idur, ispread, ifldnum, ifld1 \
-- >           [, ifld2] [...]
--
-- csound doc: <http://csound.com/docs/manual/vbap16move.html>
vbap16move :: Tuple a => Sig -> D -> D -> D -> [D] -> a
vbap16move b1 b2 b3 b4 b5 = pureTuple $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> mapM unD b5
    where f a1 a2 a3 a4 a5 = mopcs "vbap16move" ((repeat Ar),[Ar] ++ (repeat Ir)) ([a1
                                                                                   ,a2
                                                                                   ,a3
                                                                                   ,a4] ++ a5)

-- | 
-- Distributes an audio signal among 4 channels.
--
-- > ar1, ar2, ar3, ar4  vbap4  asig, kazim [, kelev] [, kspread]
--
-- csound doc: <http://csound.com/docs/manual/vbap4.html>
vbap4 ::  Sig -> Sig -> (Sig,Sig,Sig,Sig)
vbap4 b1 b2 = pureTuple $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = mopcs "vbap4" ([Ar,Ar,Ar,Ar],[Ar,Kr,Kr,Kr]) [a1,a2]

-- | 
-- Distributes an audio signal among 4 channels with moving virtual sources.
--
-- > ar1, ar2, ar3, ar4  vbap4move  asig, idur, ispread, ifldnum, ifld1 \
-- >           [, ifld2] [...]
--
-- csound doc: <http://csound.com/docs/manual/vbap4move.html>
vbap4move :: Tuple a => Sig -> D -> D -> D -> [D] -> a
vbap4move b1 b2 b3 b4 b5 = pureTuple $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> mapM unD b5
    where f a1 a2 a3 a4 a5 = mopcs "vbap4move" ([Ar,Ar,Ar,Ar],[Ar] ++ (repeat Ir)) ([a1
                                                                                    ,a2
                                                                                    ,a3
                                                                                    ,a4] ++ a5)

-- | 
-- Distributes an audio signal among 8 channels.
--
-- > ar1, ..., ar8  vbap8  asig, kazim [, kelev] [, kspread]
--
-- csound doc: <http://csound.com/docs/manual/vbap8.html>
vbap8 :: Tuple a => Sig -> Sig -> a
vbap8 b1 b2 = pureTuple $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = mopcs "vbap8" ((repeat Ar),[Ar,Kr,Kr,Kr]) [a1,a2]

-- | 
-- Distributes an audio signal among 8 channels with moving virtual sources.
--
-- > ar1, ..., ar8  vbap8move  asig, idur, ispread, ifldnum, ifld1 \
-- >           [, ifld2] [...]
--
-- csound doc: <http://csound.com/docs/manual/vbap8move.html>
vbap8move :: Tuple a => Sig -> D -> D -> D -> [D] -> a
vbap8move b1 b2 b3 b4 b5 = pureTuple $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> mapM unD b5
    where f a1 a2 a3 a4 a5 = mopcs "vbap8move" ((repeat Ar),[Ar] ++ (repeat Ir)) ([a1
                                                                                  ,a2
                                                                                  ,a3
                                                                                  ,a4] ++ a5)

-- | 
-- Calculates the gains for a sound location between multiple channels.
--
-- Calculates the gains for a sound location for up to 64.
--
-- > k1[, k2...]  vbapg  kazim [,kelev] [, kspread] [, ilayout]
-- > karray[]  vbapg  kazim [,kelev] [, kspread] [, ilayout]
--
-- csound doc: <http://csound.com/docs/manual/vbapg.html>
vbapg :: Tuple a => Sig -> a
vbapg b1 = pureTuple $ f <$> unSig b1
    where f a1 = mopcs "vbapg" ((repeat Kr),[Kr,Kr,Kr,Ir]) [a1]

-- | 
-- Calculates the gains for a sound location between multiple
--       channels with moving virtual sources.
--
-- > kr1[, kr2...]  vbapgmove  idur, ispread, ifldnum, ifld1 \
-- >           [, ifld2] [...]
-- > karray[]  vbapgmove  idur, ispread, ifldnum, ifld1 \
-- >           [, ifld2] [...]
--
-- csound doc: <http://csound.com/docs/manual/vbapgmove.html>
vbapgmove :: Tuple a => D -> D -> D -> D -> a
vbapgmove b1 b2 b3 b4 = pureTuple $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4
    where f a1 a2 a3 a4 = mopcs "vbapgmove" ((repeat Kr),(repeat Ir)) [a1,a2,a3,a4]

-- | 
-- Configures VBAP output according to loudspeaker parameters.
--
-- >  vbaplsinit  idim, ilsnum [, idir1] [, idir2] [...] [, idir32]
-- >  vbaplsinit  idim, ilsnum, ilsarray
--
-- csound doc: <http://csound.com/docs/manual/vbaplsinit.html>
vbaplsinit ::  D -> D -> SE ()
vbaplsinit b1 b2 = SE $ (depT_ =<<) $ lift $ f <$> unD b1 <*> unD b2
    where f a1 a2 = opcs "vbaplsinit" [(Xr,(repeat Ir))] [a1,a2]

-- | 
-- Distributes an audio signal among many channels with moving virtual sources.
--
-- Distributes an audio signal among upto 64 channels with moving
--       virtual sources.
--
-- > ar1[, ar2...]  vbapmove  asig, idur, ispread, ifldnum, ifld1 \
-- >           [, ifld2] [...]
-- > aarray[]  vbapmove  asig, idur, ispread, ifldnum, ifld1 \
-- >           [, ifld2] [...]
--
-- csound doc: <http://csound.com/docs/manual/vbapmove.html>
vbapmove :: Tuple a => Sig -> D -> D -> D -> [D] -> a
vbapmove b1 b2 b3 b4 b5 = pureTuple $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> mapM unD b5
    where f a1 a2 a3 a4 a5 = mopcs "vbapmove" ((repeat Ar),[Ar] ++ (repeat Ir)) ([a1
                                                                                 ,a2
                                                                                 ,a3
                                                                                 ,a4] ++ a5)

-- | 
-- Writes a multi-channel audio signal to a ZAK array.
--
-- >  vbapz  inumchnls, istartndx, asig, kazim [, kelev] [, kspread]
--
-- csound doc: <http://csound.com/docs/manual/vbapz.html>
vbapz ::  D -> D -> Sig -> Sig -> SE ()
vbapz b1 b2 b3 b4 = SE $ (depT_ =<<) $ lift $ f <$> unD b1 <*> unD b2 <*> unSig b3 <*> unSig b4
    where f a1 a2 a3 a4 = opcs "vbapz" [(Xr,[Ir,Ir,Ar,Kr,Kr,Kr])] [a1,a2,a3,a4]

-- | 
-- Writes a multi-channel audio signal to a ZAK array with moving virtual sources.
--
-- >  vbapzmove  inumchnls, istartndx, asig, idur, ispread, ifldnum, ifld1, \
-- >           ifld2, [...]
--
-- csound doc: <http://csound.com/docs/manual/vbapzmove.html>
vbapzmove ::  Sig -> D -> D -> D -> [D] -> SE ()
vbapzmove b1 b2 b3 b4 b5 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> mapM unD b5
    where f a1 a2 a3 a4 a5 = opcs "vbapzmove" [(Xr,[Ir,Ir,Ar] ++ (repeat Ir))] ([a1,a2,a3,a4] ++ a5)

-- Reverberation.

-- | 
-- Reverberates an input signal with a flat frequency response.
--
-- > ares  alpass  asig, xrvt, ilpt [, iskip] [, insmps]
--
-- csound doc: <http://csound.com/docs/manual/alpass.html>
alpass ::  Sig -> Sig -> D -> Sig
alpass b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3
    where f a1 a2 a3 = opcs "alpass" [(Ar,[Ar,Xr,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- A physical model reverberator.
--
-- babo  stands  for  ball-within-the-box.   It is a physical model reverberator based on the paper by Davide  Rocchesso "The  Ball  within  the Box: a sound-processing metaphor", Computer Music Journal,  Vol  19,  N.4,  pp.45-47,  Winter 1995.
--
-- > a1, a2  babo  asig, ksrcx, ksrcy, ksrcz, irx, iry, irz [, idiff] [, ifno]
--
-- csound doc: <http://csound.com/docs/manual/babo.html>
babo ::  Sig -> Sig -> Sig -> Sig -> D -> D -> D -> (Sig,Sig)
babo b1 b2 b3 b4 b5 b6 b7 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unD b5 <*> unD b6 <*> unD b7
    where f a1 a2 a3 a4 a5 a6 a7 = mopcs "babo" ([Ar,Ar],[Ar,Kr,Kr,Kr,Ir,Ir,Ir,Ir,Ir]) [a1
                                                                                       ,a2
                                                                                       ,a3
                                                                                       ,a4
                                                                                       ,a5
                                                                                       ,a6
                                                                                       ,a7]

-- | 
-- Reverberates an input signal with a âcoloredâ frequency response.
--
-- > ares  comb  asig, krvt, ilpt [, iskip] [, insmps]
--
-- csound doc: <http://csound.com/docs/manual/comb.html>
comb ::  Sig -> Sig -> D -> Sig
comb b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3
    where f a1 a2 a3 = opcs "comb" [(Ar,[Ar,Kr,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Reverberates an input signal with a âcoloredâ frequency response.
--
-- Reverberates an input signal with a âcoloredâ
--       frequency response with a FIR filter.
--
-- > ares  combinv  asig, krvt, ilpt [, iskip] [, insmps]
--
-- csound doc: <http://csound.com/docs/manual/combinv.html>
combinv ::  Sig -> Sig -> D -> Sig
combinv b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3
    where f a1 a2 a3 = opcs "combinv" [(Ar,[Ar,Kr,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Opcode version of Jezar's Freeverb
--
-- freeverb is a stereo reverb unit based on Jezar's public domain
-- 		C++ sources, composed of eight parallel comb filters on both
-- 		channels, followed by four allpass units in series. The filters
-- 		on the right channel are slightly detuned compared to the left
-- 		channel in order to create a stereo effect.
--
-- > aoutL, aoutR  freeverb  ainL, ainR, kRoomSize, kHFDamp[, iSRate[, iSkip]] 
--
-- csound doc: <http://csound.com/docs/manual/freeverb.html>
freeverb ::  Sig -> Sig -> Sig -> Sig -> (Sig,Sig)
freeverb b1 b2 b3 b4 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
    where f a1 a2 a3 a4 = mopcs "freeverb" ([Ar,Ar],[Ar,Ar,Kr,Kr,Ir,Ir]) [a1,a2,a3,a4]

-- | 
-- Three different nested all-pass filters.
--
-- Three different nested all-pass filters, useful for implementing reverbs.
--
-- > ares  nestedap  asig, imode, imaxdel, idel1, igain1 [, idel2] [, igain2] \
-- >           [, idel3] [, igain3] [, istor]
--
-- csound doc: <http://csound.com/docs/manual/nestedap.html>
nestedap ::  Sig -> D -> D -> D -> D -> Sig
nestedap b1 b2 b3 b4 b5 = Sig $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5
    where f a1 a2 a3 a4 a5 = opcs "nestedap" [(Ar,[Ar,Ir,Ir,Ir,Ir,Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3,a4,a5]

-- | 
-- A reverberator consisting of 6 parallel comb-lowpass filters.
--
-- This is a reverberator consisting of 6 parallel comb-lowpass filters being fed into a series of 5 allpass filters. nreverb replaces reverb2 (version 3.48) and so both opcodes are identical.
--
-- > ares  nreverb  asig, ktime, khdif [, iskip] [,inumCombs] [, ifnCombs] \
-- >           [, inumAlpas] [, ifnAlpas]
--
-- csound doc: <http://csound.com/docs/manual/nreverb.html>
nreverb ::  Sig -> Sig -> Sig -> Sig
nreverb b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "nreverb" [(Ar,[Ar,Kr,Kr,Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Models the reverberation of a metal plate.
--
-- Models the reverberation of a rectangular metal plate with
--       settable physical characteristics when excited by audio signal(s).
--
-- > a1[, a2, ...]  platerev  itabexcite. itabouts, kbndry, iaspect, istiff, idecay, iloss, aexcite1[, aexcite2, ...]
--
-- csound doc: <http://csound.com/docs/manual/platerev.html>
platerev :: Tuple a => D -> D -> Sig -> D -> D -> D -> D -> [Sig] -> a
platerev b1 b2 b3 b4 b5 b6 b7 b8 = pureTuple $ f <$> unD b1 <*> unD b2 <*> unSig b3 <*> unD b4 <*> unD b5 <*> unD b6 <*> unD b7 <*> mapM unSig b8
    where f a1 a2 a3 a4 a5 a6 a7 a8 = mopcs "platerev" ((repeat Ar)
                                                       ,[Ir,Ir,Kr,Ir,Ir,Ir,Ir] ++ (repeat Ar)) ([a1,a2,a3,a4,a5,a6,a7] ++ a8)

-- | 
-- Reverberates an input signal with a ânatural roomâ frequency response.
--
-- > ares  reverb  asig, krvt [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/reverb.html>
reverb ::  Sig -> Sig -> Sig
reverb b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "reverb" [(Ar,[Ar,Kr,Ir])] [a1,a2]

-- | 
-- Same as the nreverb opcode.
--
-- > ares  reverb2  asig, ktime, khdif [, iskip] [,inumCombs] \
-- >           [, ifnCombs] [, inumAlpas] [, ifnAlpas]
--
-- csound doc: <http://csound.com/docs/manual/reverb2.html>
reverb2 ::  Sig -> Sig -> Sig -> Sig
reverb2 b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "reverb2" [(Ar,[Ar,Kr,Kr,Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- 8 delay line stereo FDN reverb, based on work by Sean Costello
--
-- 8 delay line stereo FDN reverb, with feedback matrix based upon physical
-- 		modeling scattering junction of 8 lossless waveguides of equal characteristic
-- 		impedance. Based on Csound orchestra version by Sean Costello.
--
-- > aoutL, aoutR  reverbsc  ainL, ainR, kfblvl, kfco[, israte[, ipitchm[, iskip]]] 
--
-- csound doc: <http://csound.com/docs/manual/reverbsc.html>
reverbsc ::  Sig -> Sig -> Sig -> Sig -> (Sig,Sig)
reverbsc b1 b2 b3 b4 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
    where f a1 a2 a3 a4 = mopcs "reverbsc" ([Ar,Ar],[Ar,Ar,Kr,Kr,Ir,Ir,Ir]) [a1,a2,a3,a4]

-- | 
-- Variably reverberates an input signal with a flat frequency response.
--
-- > ares  valpass  asig, krvt, xlpt, imaxlpt [, iskip] [, insmps]
--
-- csound doc: <http://csound.com/docs/manual/valpass.html>
valpass ::  Sig -> Sig -> Sig -> D -> Sig
valpass b1 b2 b3 b4 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4
    where f a1 a2 a3 a4 = opcs "valpass" [(Ar,[Ar,Kr,Xr,Ir,Ir,Ir])] [a1,a2,a3,a4]

-- | 
-- Variably reverberates an input signal with a âcoloredâ frequency response.
--
-- > ares  vcomb  asig, krvt, xlpt, imaxlpt [, iskip] [, insmps]
--
-- csound doc: <http://csound.com/docs/manual/vcomb.html>
vcomb ::  Sig -> Sig -> Sig -> D -> Sig
vcomb b1 b2 b3 b4 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4
    where f a1 a2 a3 a4 = opcs "vcomb" [(Ar,[Ar,Kr,Xr,Ir,Ir,Ir])] [a1,a2,a3,a4]

-- Sample Level Operators.

-- | 
-- Mixes low level noise to a list of a-rate signals
--
-- Mixes low level (~1e-20 for floats, and ~1e-56 for doubles)
--     	noise to a list of a-rate signals. Can be used before IIR
--     	filters and reverbs to avoid denormalized numbers which may
--     	otherwise result in significantly increased CPU usage.
--
-- >  denorm  a1[, a2[, a3[, ... ]]]
--
-- csound doc: <http://csound.com/docs/manual/denorm.html>
denorm ::  [Sig] -> SE ()
denorm b1 = SE $ (depT_ =<<) $ lift $ f <$> mapM unSig b1
    where f a1 = opcs "denorm" [(Xr,(repeat Ar))] a1

-- | 
-- Modify a signal by differentiation.
--
-- > ares  diff  asig [, iskip]
-- > kres  diff  ksig [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/diff.html>
diff ::  Sig -> Sig
diff b1 = Sig $ f <$> unSig b1
    where f a1 = opcs "diff" [(Ar,[Ar,Ir]),(Kr,[Kr,Ir])] [a1]

-- | 
-- Modify a signal by down-sampling.
--
-- > kres  downsamp  asig [, iwlen]
--
-- csound doc: <http://csound.com/docs/manual/downsamp.html>
downsamp ::  Sig -> Sig
downsamp b1 = Sig $ f <$> unSig b1
    where f a1 = opcs "downsamp" [(Kr,[Ar,Ir])] [a1]

-- | 
-- Adds artificial foldover to an audio signal.
--
-- > ares  fold  asig, kincr
--
-- csound doc: <http://csound.com/docs/manual/fold.html>
fold ::  Sig -> Sig -> Sig
fold b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "fold" [(Ar,[Ar,Kr])] [a1,a2]

-- | 
-- Modify a signal by integration.
--
-- > ares  integ  asig [, iskip]
-- > kres  integ  ksig [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/integ.html>
integ ::  Sig -> Sig
integ b1 = Sig $ f <$> unSig b1
    where f a1 = opcs "integ" [(Ar,[Ar,Ir]),(Kr,[Kr,Ir])] [a1]

-- | 
-- Converts a control signal to an audio signal using linear interpolation.
--
-- > ares  interp  ksig [, iskip] [, imode]
-- >         [, ivalue]
--
-- csound doc: <http://csound.com/docs/manual/interp.html>
interp ::  Sig -> Sig
interp b1 = Sig $ f <$> unSig b1
    where f a1 = opcs "interp" [(Ar,[Kr,Ir,Ir,Ir])] [a1]

-- | 
-- Calculates the weighted mean value of two input signals.
--
-- Calculates the weighted mean value (i.e. linear interpolation) of two input signals
--
-- > ares  ntrpol  asig1, asig2, kpoint [, imin] [, imax]
-- > ires  ntrpol  isig1, isig2, ipoint [, imin] [, imax]
-- > kres  ntrpol  ksig1, ksig2, kpoint [, imin] [, imax]
--
-- csound doc: <http://csound.com/docs/manual/ntrpol.html>
ntrpol ::  Sig -> Sig -> Sig -> Sig
ntrpol b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "ntrpol" [(Ar,[Ar,Ar,Kr,Ir,Ir])
                                     ,(Ir,[Ir,Ir,Ir,Ir,Ir])
                                     ,(Kr,[Kr,Kr,Kr,Ir,Ir])] [a1,a2,a3]

-- | 
-- Performs a sample-and-hold operation on its input.
--
-- > ares  samphold  asig, agate [, ival] [, ivstor]
-- > kres  samphold  ksig, kgate [, ival] [, ivstor]
--
-- csound doc: <http://csound.com/docs/manual/samphold.html>
samphold ::  Sig -> Sig -> Sig
samphold b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "samphold" [(Ar,[Ar,Ar,Ir,Ir]),(Kr,[Kr,Kr,Ir,Ir])] [a1,a2]

-- | 
-- Modify a signal by up-sampling.
--
-- > ares  upsamp  ksig
--
-- csound doc: <http://csound.com/docs/manual/upsamp.html>
upsamp ::  Sig -> Sig
upsamp b1 = Sig $ f <$> unSig b1
    where f a1 = opcs "upsamp" [(Ar,[Kr])] [a1]

-- | 
-- Access values of the current buffer of an a-rate variable by indexing.
--
-- Access values of the current buffer of an a-rate variable by indexing.
--       Useful for doing sample-by-sample manipulation at k-rate without using
--       setksmps 1.
--
-- > kval  vaget  kndx, avar
--
-- csound doc: <http://csound.com/docs/manual/vaget.html>
vaget ::  Sig -> Sig -> Sig
vaget b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "vaget" [(Kr,[Kr,Ar])] [a1,a2]

-- | 
-- Write value of into the current buffer of an a-rate variable by index.
--
-- Write values into the current buffer of an a-rate variable at the given
--       index.  Useful for doing sample-by-sample manipulation at k-rate without
--       using setksmps 1.
--
-- >  vaset  kval, kndx, avar
--
-- csound doc: <http://csound.com/docs/manual/vaset.html>
vaset ::  Sig -> Sig -> Sig -> SE ()
vaset b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "vaset" [(Xr,[Kr,Kr,Ar])] [a1,a2,a3]

-- Signal Limiters.

-- | 
-- Sets the lower and upper limits of the value it processes.
--
-- > ares  limit  asig, klow, khigh
-- > ires  limit  isig, ilow, ihigh
-- > kres  limit  ksig, klow, khigh
-- > ires[]  limit  isig[], ilow, ihigh
-- > kres[]  limit  ksig[], klow, khigh
--
-- csound doc: <http://csound.com/docs/manual/limit.html>
limit ::  Sig -> Sig -> Sig -> Sig
limit b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "limit" [(Ar,[Ar,Kr,Kr])
                                    ,(Ir,[Ir,Ir,Ir])
                                    ,(Kr,[Kr,Kr,Kr])
                                    ,(Ir,[Ir,Ir,Ir])
                                    ,(Kr,[Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Reflects the signal that exceeds the low and high thresholds.
--
-- > ares  mirror  asig, klow, khigh
-- > ires  mirror  isig, ilow, ihigh
-- > kres  mirror  ksig, klow, khigh
--
-- csound doc: <http://csound.com/docs/manual/mirror.html>
mirror ::  Sig -> Sig -> Sig -> Sig
mirror b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "mirror" [(Ar,[Ar,Kr,Kr]),(Ir,[Ir,Ir,Ir]),(Kr,[Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Wraps-around the signal that exceeds the low and high thresholds.
--
-- > ares  wrap  asig, klow, khigh
-- > ires  wrap  isig, ilow, ihigh
-- > kres  wrap  ksig, klow, khigh
--
-- csound doc: <http://csound.com/docs/manual/wrap.html>
wrap ::  Sig -> Sig -> Sig -> Sig
wrap b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "wrap" [(Ar,[Ar,Kr,Kr]),(Ir,[Ir,Ir,Ir]),(Kr,[Kr,Kr,Kr])] [a1,a2,a3]

-- Special Effects.

-- | 
-- Distort an audio signal via waveshaping and optional clipping.
--
-- > ar  distort  asig, kdist, ifn[, ihp, istor]
--
-- csound doc: <http://csound.com/docs/manual/distort.html>
distort ::  Sig -> Sig -> Tab -> Sig
distort b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unTab b3
    where f a1 a2 a3 = opcs "distort" [(Ar,[Ar,Kr,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Modified hyperbolic tangent distortion.
--
-- Implementation of modified hyperbolic tangent distortion. distort1 can be used to generate wave shaping distortion based on a modification of the tanh function.
--
-- > ares  distort1  asig, kpregain, kpostgain, kshape1, kshape2[, imode]
--
-- csound doc: <http://csound.com/docs/manual/distort1.html>
distort1 ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig
distort1 b1 b2 b3 b4 b5 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5
    where f a1 a2 a3 a4 a5 = opcs "distort1" [(Ar,[Ar,Kr,Kr,Kr,Kr,Ir])] [a1,a2,a3,a4,a5]

-- | 
-- A user controlled flanger.
--
-- > ares  flanger  asig, adel, kfeedback [, imaxd]
--
-- csound doc: <http://csound.com/docs/manual/flanger.html>
flanger ::  Sig -> Sig -> Sig -> Sig
flanger b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "flanger" [(Ar,[Ar,Ar,Kr,Ir])] [a1,a2,a3]

-- | 
-- Analyze an audio input and generate harmonizing voices in synchrony.
--
-- > ares  harmon  asig, kestfrq, kmaxvar, kgenfreq1, kgenfreq2, imode, \
-- >           iminfrq, iprd
--
-- csound doc: <http://csound.com/docs/manual/harmon.html>
harmon ::  Sig -> Sig -> Sig -> Sig -> Sig -> D -> D -> D -> Sig
harmon b1 b2 b3 b4 b5 b6 b7 b8 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unD b6 <*> unD b7 <*> unD b8
    where f a1 a2 a3 a4 a5 a6 a7 a8 = opcs "harmon" [(Ar,[Ar,Kr,Kr,Kr,Kr,Ir,Ir,Ir])] [a1
                                                                                     ,a2
                                                                                     ,a3
                                                                                     ,a4
                                                                                     ,a5
                                                                                     ,a6
                                                                                     ,a7
                                                                                     ,a8]

-- | 
-- Analyze an audio input and generate harmonizing voices in
--       synchrony with formants preserved.
--
-- Generate harmonizing voices with formants preserved.
--
-- > ares  harmon2  asig, koct, kfrq1, kfrq2, icpsmode, ilowest[, ipolarity]
--
-- csound doc: <http://csound.com/docs/manual/harmon2.html>
harmon2 ::  Sig -> Sig -> Sig -> Sig -> D -> D -> Sig
harmon2 b1 b2 b3 b4 b5 b6 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unD b5 <*> unD b6
    where f a1 a2 a3 a4 a5 a6 = opcs "harmon2" [(Ar,[Ar,Kr,Kr,Kr,Ir,Ir,Ir])] [a1,a2,a3,a4,a5,a6]

-- | 
-- Analyze an audio input and generate harmonizing voices in
--       synchrony with formants preserved.
--
-- Generate harmonizing voices with formants preserved.
--
-- > ares  harmon3  asig, koct, kfrq1, \
-- >         kfrq2, kfrq3, icpsmode, ilowest[, ipolarity]
--
-- csound doc: <http://csound.com/docs/manual/harmon2.html>
harmon3 ::  Sig -> Sig -> Sig -> Sig -> Sig -> D -> D -> Sig
harmon3 b1 b2 b3 b4 b5 b6 b7 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unD b6 <*> unD b7
    where f a1 a2 a3 a4 a5 a6 a7 = opcs "harmon3" [(Ar,[Ar,Kr,Kr,Kr,Kr,Ir,Ir,Ir])] [a1
                                                                                   ,a2
                                                                                   ,a3
                                                                                   ,a4
                                                                                   ,a5
                                                                                   ,a6
                                                                                   ,a7]

-- | 
-- Analyze an audio input and generate harmonizing voices in
--       synchrony with formants preserved.
--
-- Generate harmonizing voices with formants preserved.
--
-- > ares  harmon4  asig, koct, kfrq1, \
-- >         kfrq2, kfrq3, kfrq4, icpsmode, ilowest[, ipolarity]
--
-- csound doc: <http://csound.com/docs/manual/harmon2.html>
harmon4 ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> D -> Sig
harmon4 b1 b2 b3 b4 b5 b6 b7 b8 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unD b7 <*> unD b8
    where f a1 a2 a3 a4 a5 a6 a7 a8 = opcs "harmon4" [(Ar,[Ar,Kr,Kr,Kr,Kr,Kr,Ir,Ir,Ir])] [a1
                                                                                         ,a2
                                                                                         ,a3
                                                                                         ,a4
                                                                                         ,a5
                                                                                         ,a6
                                                                                         ,a7
                                                                                         ,a8]

-- | 
-- First-order allpass filters arranged in a series.
--
-- An implementation of iord number of first-order allpass filters in series.
--
-- > ares  phaser1  asig, kfreq, kord, kfeedback [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/phaser1.html>
phaser1 ::  Sig -> Sig -> Sig -> Sig -> Sig
phaser1 b1 b2 b3 b4 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
    where f a1 a2 a3 a4 = opcs "phaser1" [(Ar,[Ar,Kr,Kr,Kr,Ir])] [a1,a2,a3,a4]

-- | 
-- Second-order allpass filters arranged in a series.
--
-- An implementation of iord number of second-order allpass filters in series.
--
-- > ares  phaser2  asig, kfreq, kq, kord, kmode, ksep, kfeedback
--
-- csound doc: <http://csound.com/docs/manual/phaser2.html>
phaser2 ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
phaser2 b1 b2 b3 b4 b5 b6 b7 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7
    where f a1 a2 a3 a4 a5 a6 a7 = opcs "phaser2" [(Ar,[Ar,Kr,Kr,Kr,Kr,Kr,Kr])] [a1
                                                                                ,a2
                                                                                ,a3
                                                                                ,a4
                                                                                ,a5
                                                                                ,a6
                                                                                ,a7]

-- Standard Filters.

-- | 
-- A hi-pass filter whose transfer functions are the complements of the tone opcode.
--
-- > ares  atone  asig, khp [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/atone.html>
atone ::  Sig -> Sig -> Sig
atone b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "atone" [(Ar,[Ar,Kr,Ir])] [a1,a2]

-- | 
-- Emulates a stack of filters using the atone opcode.
--
-- atonex is equivalent to a filter consisting of more layers of atone with the same arguments, serially connected. Using a stack of a larger number of filters allows a sharper cutoff. They are faster than using a larger number instances in a Csound orchestra of the old opcodes, because only one initialization and k- cycle are needed at time and the audio loop falls entirely inside the cache memory of processor.
--
-- > ares  atonex  asig, khp [, inumlayer] [, iskip]
-- > ares  atonex  asig, ahp [, inumlayer] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/atonex.html>
atonex ::  Sig -> Sig -> Sig
atonex b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "atonex" [(Ar,[Ar,Kr,Ir,Ir]),(Ar,[Ar,Ar,Ir,Ir])] [a1,a2]

-- | 
-- A sweepable general purpose biquadratic digital filter.
--
-- > ares  biquad  asig, kb0, kb1, kb2, ka0, ka1, ka2 [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/biquad.html>
biquad ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
biquad b1 b2 b3 b4 b5 b6 b7 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7
    where f a1 a2 a3 a4 a5 a6 a7 = opcs "biquad" [(Ar,[Ar,Kr,Kr,Kr,Kr,Kr,Kr,Ir])] [a1
                                                                                  ,a2
                                                                                  ,a3
                                                                                  ,a4
                                                                                  ,a5
                                                                                  ,a6
                                                                                  ,a7]

-- | 
-- A sweepable general purpose biquadratic digital filter with a-rate parameters.
--
-- A sweepable general purpose biquadratic digital filter.
--
-- > ares  biquada  asig, ab0, ab1, ab2, aa0, aa1, aa2 [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/biquada.html>
biquada ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
biquada b1 b2 b3 b4 b5 b6 b7 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7
    where f a1 a2 a3 a4 a5 a6 a7 = opcs "biquada" [(Ar,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ir])] [a1
                                                                                   ,a2
                                                                                   ,a3
                                                                                   ,a4
                                                                                   ,a5
                                                                                   ,a6
                                                                                   ,a7]

-- | 
-- Same as the butterbp opcode.
--
-- > ares  butbp  asig, kfreq, kband [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/butbp.html>
butbp ::  Sig -> Sig -> Sig -> Sig
butbp b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "butbp" [(Ar,[Ar,Kr,Kr,Ir])] [a1,a2,a3]

-- | 
-- Same as the butterbr opcode.
--
-- > ares  butbr  asig, kfreq, kband [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/butbr.html>
butbr ::  Sig -> Sig -> Sig -> Sig
butbr b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "butbr" [(Ar,[Ar,Kr,Kr,Ir])] [a1,a2,a3]

-- | 
-- Same as the butterhp opcode.
--
-- > ares  buthp  asig, kfreq [, iskip]
-- > ares  buthp  asig, afreq [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/buthp.html>
buthp ::  Sig -> Sig -> Sig
buthp b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "buthp" [(Ar,[Ar,Kr,Ir]),(Ar,[Ar,Ar,Ir])] [a1,a2]

-- | 
-- Same as the butterlp opcode.
--
-- > ares  butlp  asig, kfreq [, iskip]
-- > ares  butlp  asig, afreq [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/butlp.html>
butlp ::  Sig -> Sig -> Sig
butlp b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "butlp" [(Ar,[Ar,Kr,Ir]),(Ar,[Ar,Ar,Ir])] [a1,a2]

-- | 
-- A band-pass Butterworth filter.
--
-- Implementation of a second-order band-pass Butterworth filter. This opcode can also be written as butbp.
--
-- > ares  butterbp  asig, xfreq, xband [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/butterbp.html>
butterbp ::  Sig -> Sig -> Sig -> Sig
butterbp b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "butterbp" [(Ar,[Ar,Xr,Xr,Ir])] [a1,a2,a3]

-- | 
-- A band-reject Butterworth filter.
--
-- Implementation of a second-order band-reject Butterworth filter. This opcode can also be written as butbr.
--
-- > ares  butterbr  asig, xfreq, xband [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/butterbr.html>
butterbr ::  Sig -> Sig -> Sig -> Sig
butterbr b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "butterbr" [(Ar,[Ar,Xr,Xr,Ir])] [a1,a2,a3]

-- | 
-- A high-pass Butterworth filter.
--
-- Implementation of second-order high-pass Butterworth filter. This opcode can also be written as buthp.
--
-- > ares  butterhp  asig, kfreq [, iskip]
-- > ares  butterhp  asig, afreq [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/butterhp.html>
butterhp ::  Sig -> Sig -> Sig
butterhp b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "butterhp" [(Ar,[Ar,Kr,Ir]),(Ar,[Ar,Ar,Ir])] [a1,a2]

-- | 
-- A low-pass Butterworth filter.
--
-- Implementation of a second-order low-pass Butterworth filter. This opcode can also be written as butlp.
--
-- > ares  butterlp  asig, kfreq [, iskip]
-- > ares  butterlp  asig, afreq [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/butterlp.html>
butterlp ::  Sig -> Sig -> Sig
butterlp b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "butterlp" [(Ar,[Ar,Kr,Ir]),(Ar,[Ar,Ar,Ir])] [a1,a2]

-- | 
-- Implements low-pass and high-pass filters of different styles.
--
-- Implements the classical standard analog filter types: low-pass and high-pass. They are implemented with the four classical kinds of filters: Butterworth, Chebyshev Type I, Chebyshev Type II, and Elliptical.  The number of poles may be any even number from 2 to 80.
--
-- > ares  clfilt  asig, kfreq, itype, inpol [, ikind] [, ipbr] [, isba] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/clfilt.html>
clfilt ::  Sig -> Sig -> D -> D -> Sig
clfilt b1 b2 b3 b4 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unD b4
    where f a1 a2 a3 a4 = opcs "clfilt" [(Ar,[Ar,Kr,Ir,Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3,a4]

-- | 
-- Zero-delay feedback implementation of 4 pole diode ladder filter.
--
-- Zero-delay feedback implementation of a 4 pole (24 dB/oct) diode low-pass filter. This filter design was originally used in the EMS VCS3 and was the resonant filter in the Roland TB-303.
--
-- > asig  diode_ladder  ain, xcf, xk [, inlp, isaturation, istor]
--
-- csound doc: <http://csound.com/docs/manual/diode_ladder.html>
diode_ladder ::  Sig -> Sig -> Sig -> Sig
diode_ladder b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "diode_ladder" [(Ar,[Ar,Xr,Xr,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- A fast and robust method for approximating sound propagation, achieving convincing Doppler shifts without having to solve equations.
--
-- A fast and robust method for approximating sound propagation, achieving convincing Doppler shifts without having to solve equations. The method computes frequency shifts based on reading an input delay line at a delay time computed from the distance between source and mic and the speed of sound. One instance of the opcode is required for each dimension of space through which the sound source moves. If the source sound moves at a constant speed from in front of the microphone, through the microphone, to behind the microphone, then the output will be frequency shifted above the source frequency at a constant frequency while the source approaches, then discontinuously will be shifted below the source frequency at a constant frequency as the source recedes from the microphone. If the source sound moves at a constant speed through a point to one side of the microphone, then the rate of change of position will not be constant, and the familiar Doppler frequency shift typical of a siren or engine approaching and receding along a road beside a listener will be heard.
--
-- > ashifted  doppler  asource, ksourceposition, kmicposition [, isoundspeed, ifiltercutoff]
--
-- csound doc: <http://csound.com/docs/manual/doppler.html>
doppler ::  Sig -> Sig -> Sig -> Sig
doppler b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "doppler" [(Ar,[Ar,Kr,Kr,Ir,Ir])] [a1,a2,a3]

-- | 
-- Zero-delay feedback implementation of Korg35 resonant high-pass filter.
--
-- Zero-delay feedback implementation of Korg35 resonant high-pass filter. This filter design is found in the Korg MS10 early MS20.
--
-- > asig  K35_hpf  ain, xcf, xQ [, inlp, isaturation, istor]
--
-- csound doc: <http://csound.com/docs/manual/k35_hpf.html>
k35_hpf ::  Sig -> Sig -> Sig -> Sig
k35_hpf b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "K35_hpf" [(Ar,[Ar,Xr,Xr,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Zero-delay feedback implementation of Korg35 resonant low-pass filter.
--
-- Zero-delay feedback implementation of Korg35 resonant low-pass filter. This filter design is found in the Korg MS10, early MS20, and Monotron series.
--
-- > asig  K35_lpf  ain, xcf, xQ [, inlp, isaturation, istor]
--
-- csound doc: <http://csound.com/docs/manual/k35_lpf.html>
k35_lpf ::  Sig -> Sig -> Sig -> Sig
k35_lpf b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "K35_lpf" [(Ar,[Ar,Xr,Xr,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- A median filter, a variant FIR lowpass filter.
--
-- Implementation of a median filter.
--
-- > ares  median  asig, ksize, imaxsize [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/median.html>
median ::  Sig -> Sig -> D -> Sig
median b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3
    where f a1 a2 a3 = opcs "median" [(Ar,[Ar,Kr,Ir,Ir])] [a1,a2,a3]

-- | 
-- A median filter, a variant FIR lowpass filter.
--
-- Implementation of a median filter.
--
-- > kres  mediank  kin, ksize, imaxsize [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/mediank.html>
mediank ::  Sig -> Sig -> D -> Sig
mediank b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3
    where f a1 a2 a3 = opcs "mediank" [(Kr,[Kr,Kr,Ir,Ir])] [a1,a2,a3]

-- | 
-- A filter that simulates a mass-spring-damper system
--
-- Filters the incoming signal with the specified resonance frequency and
--       quality factor. It can also be seen as a signal generator for high quality
--       factor, with an impulse for the excitation. You can combine several modes
--       to built complex instruments such as bells or guitar tables.
--
-- > aout  mode  ain, xfreq, xQ [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/mode.html>
mode ::  Sig -> Sig -> Sig -> Sig
mode b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "mode" [(Ar,[Ar,Xr,Xr,Ir])] [a1,a2,a3]

-- | 
-- A first-order recursive low-pass filter with variable frequency response.
--
-- > ares  tone  asig, khp [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/tone.html>
tone ::  Sig -> Sig -> Sig
tone b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "tone" [(Ar,[Ar,Kr,Ir])] [a1,a2]

-- | 
-- Emulates a stack of filters using the tone opcode.
--
-- tonex is equivalent to a filter consisting of more layers of tone with the same arguments, serially connected. Using a stack of a larger number of filters allows a sharper cutoff. They are faster than using a larger number instances in a Csound orchestra of the old opcodes, because only one initialization and k- cycle are needed at time and the audio loop falls entirely inside the cache memory of processor.
--
-- > ares  tonex   asig, khp [, inumlayer] [, iskip]
-- > ares  tonex   asig, ahp [, inumlayer] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/tonex.html>
tonex ::  Sig -> Sig -> Sig
tonex b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "tonex" [(Ar,[Ar,Kr,Ir,Ir]),(Ar,[Ar,Ar,Ir,Ir])] [a1,a2]

-- | 
-- Zero-delay feedback implementation of 1 pole filter.
--
-- Zero-delay feedback implementation of a 1 pole (6 dB/oct) filter. Offers low-pass (default), high-pass, and allpass output modes.
--
-- > asig  zdf_1pole  ain, xcf [, kmode, istor]
--
-- csound doc: <http://csound.com/docs/manual/zdf_1pole.html>
zdf_1pole ::  Sig -> Sig -> Sig
zdf_1pole b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "zdf_1pole" [(Ar,[Ar,Xr,Kr,Ir])] [a1,a2]

-- | 
-- Zero-delay feedback implementation of 1 pole filter with multimode output.
--
-- Zero-delay feedback implementation of a 1 pole (6 dB/oct) filter. Offers low-pass and high-pass output.
--
-- > alp, ahp  zdf_1pole_mode  ain, xcf [, istor]
--
-- csound doc: <http://csound.com/docs/manual/zdf_1pole_mode.html>
zdf_1pole_mode ::  Sig -> Sig -> (Sig,Sig)
zdf_1pole_mode b1 b2 = pureTuple $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = mopcs "zdf_1pole_mode" ([Ar,Ar],[Ar,Xr,Ir]) [a1,a2]

-- | 
-- Zero-delay feedback implementation of 2 pole filter.
--
-- Zero-delay feedback implementation of a 2 pole (12 dB/oct) filter. Offers low-pass (default), high-pass, and allpass output modes.
--
-- > asig  zdf_2pole  ain, xcf, xQ [, kmode, istor]
--
-- csound doc: <http://csound.com/docs/manual/zdf_2pole.html>
zdf_2pole ::  Sig -> Sig -> Sig -> Sig
zdf_2pole b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "zdf_2pole" [(Ar,[Ar,Xr,Xr,Kr,Ir])] [a1,a2,a3]

-- | 
-- Zero-delay feedback implementation of 2 pole filter with multimode output.
--
-- Zero-delay feedback implementation of a 2 pole (12 dB/oct) filter. Offers low-pass,
--       band-pass, and high-pass output.
--
-- > alp, abp, ahp  zdf_2pole_mode  ain, xcf, Q [, istor]
--
-- csound doc: <http://csound.com/docs/manual/zdf_2pole_mode.html>
zdf_2pole_mode ::  Sig -> Sig -> Sig -> (Sig,Sig,Sig)
zdf_2pole_mode b1 b2 b3 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = mopcs "zdf_2pole_mode" ([Ar,Ar,Ar],[Ar,Xr,Xr,Ir]) [a1,a2,a3]

-- | 
-- Zero-delay feedback implementation of 4 pole ladder filter.
--
-- Zero-delay feedback implementation of a 4 pole (24 dB/oct) low-pass filter based on the Moog ladder filter.
--
-- > asig  zdf_ladder  ain, xcf, xQ [, istor]
--
-- csound doc: <http://csound.com/docs/manual/zdf_ladder.html>
zdf_ladder ::  Sig -> Sig -> Sig -> Sig
zdf_ladder b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "zdf_ladder" [(Ar,[Ar,Xr,Xr,Ir])] [a1,a2,a3]

-- Standard Filters:Resonant.

-- | 
-- A notch filter whose transfer functions are the complements of
--       the reson opcode.
--
-- > ares  areson  asig, kcf, kbw [, iscl] [, iskip]
-- > ares  areson  asig, acf, kbw [, iscl] [, iskip]
-- > ares  areson  asig, kcf, abw [, iscl] [, iskip]
-- > ares  areson  asig, acf, abw [, iscl] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/areson.html>
areson ::  Sig -> Sig -> Sig -> Sig
areson b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "areson" [(Ar,[Ar,Kr,Kr,Ir,Ir])
                                     ,(Ar,[Ar,Ar,Kr,Ir,Ir])
                                     ,(Ar,[Ar,Kr,Ar,Ir,Ir])
                                     ,(Ar,[Ar,Ar,Ar,Ir,Ir])] [a1,a2,a3]

-- | 
-- A second-order multi-mode filter.
--
-- > ares  bqrez  asig, xfco, xres [, imode] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/bqrez.html>
bqrez ::  Sig -> Sig -> Sig -> Sig
bqrez b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "bqrez" [(Ar,[Ar,Xr,Xr,Ir,Ir])] [a1,a2,a3]

-- | 
-- A resonant lowpass filter.
--
-- Implementation of a resonant second-order lowpass filter.
--
-- > ares  lowpass2  asig, kcf, kq [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/lowpass2.html>
lowpass2 ::  Sig -> Sig -> Sig -> Sig
lowpass2 b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "lowpass2" [(Ar,[Ar,Kr,Kr,Ir])] [a1,a2,a3]

-- | 
-- Another resonant lowpass filter.
--
-- lowres is a resonant lowpass filter.
--
-- > ares  lowres  asig, kcutoff, kresonance [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/lowres.html>
lowres ::  Sig -> Sig -> Sig -> Sig
lowres b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "lowres" [(Ar,[Ar,Kr,Kr,Ir])] [a1,a2,a3]

-- | 
-- Simulates layers of serially connected resonant lowpass filters.
--
-- lowresx is equivalent to more layers of lowres with the same arguments serially connected.
--
-- > ares  lowresx  asig, xcutoff, xresonance [, inumlayer] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/lowresx.html>
lowresx ::  Sig -> Sig -> Sig -> Sig
lowresx b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "lowresx" [(Ar,[Ar,Xr,Xr,Ir,Ir])] [a1,a2,a3]

-- | 
-- A 3-pole sweepable resonant lowpass filter.
--
-- Implementation of a 3 pole sweepable resonant lowpass filter.
--
-- > ares  lpf18  asig, xfco, xres, xdist [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/lpf18.html>
lpf18 ::  Sig -> Sig -> Sig -> Sig -> Sig
lpf18 b1 b2 b3 b4 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
    where f a1 a2 a3 a4 = opcs "lpf18" [(Ar,[Ar,Xr,Xr,Xr,Ir])] [a1,a2,a3,a4]

-- | 
-- Moog ladder lowpass filter.
--
-- Moogladder is an new digital implementation of the Moog ladder filter based on 
-- the work of Antti Huovilainen, described in the paper "Non-Linear Digital 
-- Implementation of the Moog Ladder Filter" (Proceedings of DaFX04, Univ of Napoli). 
-- This implementation is probably a more accurate digital representation of 
-- the original analogue filter.
--
-- > asig  moogladder  ain, kcf, kres[, istor]
-- > asig  moogladder  ain, acf, kres[, istor]
-- > asig  moogladder  ain, kcf, ares[, istor]
-- > asig  moogladder  ain, acf, ares[, istor]
--
-- csound doc: <http://csound.com/docs/manual/moogladder.html>
moogladder ::  Sig -> Sig -> Sig -> Sig
moogladder b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "moogladder" [(Ar,[Ar,Kr,Kr,Ir])
                                         ,(Ar,[Ar,Ar,Kr,Ir])
                                         ,(Ar,[Ar,Kr,Ar,Ir])
                                         ,(Ar,[Ar,Ar,Ar,Ir])] [a1,a2,a3]

-- | 
-- Moog ladder lowpass filter.
--
-- Moogladder2 is an new digital implementation of the Moog ladder filter based on 
-- the work of Antti Huovilainen, described in the paper "Non-Linear Digital 
-- Implementation of the Moog Ladder Filter" (Proceedings of DaFX04, Univ of Napoli). 
-- This implementation uses approximations to the tanh function and so is
-- faster but less accurate than moogladder.
--
-- > asig  moogladder2  ain, kcf, kres[, istor]
-- > asig  moogladder2  ain, acf, kres[, istor]
-- > asig  moogladder2  ain, kcf, ares[, istor]
-- > asig  moogladder2  ain, acf, ares[, istor]
--
-- csound doc: <http://csound.com/docs/manual/moogladder2.html>
moogladder2 ::  Sig -> Sig -> Sig -> Sig
moogladder2 b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "moogladder2" [(Ar,[Ar,Kr,Kr,Ir])
                                          ,(Ar,[Ar,Ar,Kr,Ir])
                                          ,(Ar,[Ar,Kr,Ar,Ir])
                                          ,(Ar,[Ar,Ar,Ar,Ir])] [a1,a2,a3]

-- | 
-- A digital emulation of the Moog diode ladder filter configuration.
--
-- > ares  moogvcf  asig, xfco, xres [,iscale, iskip]
--
-- csound doc: <http://csound.com/docs/manual/moogvcf.html>
moogvcf ::  Sig -> Sig -> Sig -> Sig
moogvcf b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "moogvcf" [(Ar,[Ar,Xr,Xr,Ir,Ir])] [a1,a2,a3]

-- | 
-- A digital emulation of the Moog diode ladder filter configuration.
--
-- > ares  moogvcf2  asig, xfco, xres [,iscale, iskip]
--
-- csound doc: <http://csound.com/docs/manual/moogvcf2.html>
moogvcf2 ::  Sig -> Sig -> Sig -> Sig
moogvcf2 b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "moogvcf2" [(Ar,[Ar,Xr,Xr,Ir,Ir])] [a1,a2,a3]

-- | 
-- Moog voltage-controlled highpass filter emulation.
--
-- Mvchpf is an digital implementation of the 4th-order (24 dB/oct)  Moog
-- high-pass filter, originally written by Fons Andriaensen. According to the author,
-- mvchpf "...is based on the voltage controlled highpass filter by Robert Moog.
-- again with some attention to the nonlinear effects."
--
-- > asig  mvchpf  ain, xcf[, istor]
--
-- csound doc: <http://csound.com/docs/manual/mvchpf.html>
mvchpf ::  Sig -> Sig -> Sig
mvchpf b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "mvchpf" [(Ar,[Ar,Xr,Ir])] [a1,a2]

-- | 
-- Moog voltage-controlled lowpass filter emulation.
--
-- Mvclpf1 is an digital implementation of the 4th-order (24 dB/oct)  Moog ladder filter
-- originally written by Fons Andriaensen. According to the author,
-- mvclpf1 "is a fairly simple design, and it does not even pretend to come
-- close the 'real thing'. It uses a very crude approximation of the non-linear
-- resistor in the first filter section only. [...] [I]t's [a] cheap (in
-- terms of CPU usage) general purpose 24 dB/oct lowpass
-- filter that could be useful".
--
-- > asig  mvclpf1  ain, xcf, xres[,istor]
--
-- csound doc: <http://csound.com/docs/manual/mvclpf1.html>
mvclpf1 ::  Sig -> Sig -> Sig -> Sig
mvclpf1 b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "mvclpf1" [(Ar,[Ar,Xr,Xr,Ir])] [a1,a2,a3]

-- | 
-- Moog voltage-controlled lowpass filter emulation.
--
-- Mvclpf2 is an digital implementation of the 4th-order (24 dB/oct) Moog ladder filter
-- originally written by Fons Andriaensen. According to the author,
-- mvclpf2 "uses five non-linear elements, in the input and in all four filter
-- sections. It works by using the derivative of the nonlinearity (for which
-- 1 / (1 + x * x) is reasonable approximation). The main advantage of this is
-- that only one evaluation of the non-linear function is required for each
-- section".
--
-- > asig  mvclpf2  ain, xcf, xres[, istor]
--
-- csound doc: <http://csound.com/docs/manual/mvclpf2.html>
mvclpf2 ::  Sig -> Sig -> Sig -> Sig
mvclpf2 b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "mvclpf2" [(Ar,[Ar,Xr,Xr,Ir])] [a1,a2,a3]

-- | 
-- Moog voltage-controlled lowpass filter emulation.
--
-- Mvclpf3 is an digital implementation of the 4th-order (24 dB/oct) Moog ladder filter
-- originally written by Fons Andriaensen. According to the author,
-- mvclpf3 "is based on mvclpf2 , with two differences. It uses the
-- the technique described by Stilson and Smith to extend the constant-Q
-- range, and the internal sample frequency is doubled, giving a better
-- approximation to the non-linear behaviour at high freqencies.
-- This version has high Q over the entire frequency range and will
-- oscillate up to above 10 kHz, while the two others show a decreasing
-- Q at high frequencies. Mvclpf3  is reasonably well tuned, and can be
-- 'played' as a VCO up to at least 5 kHz".
--
-- > asig  mvclpf3  ain, xcf, xres[, istor]
--
-- csound doc: <http://csound.com/docs/manual/mvclpf3.html>
mvclpf3 ::  Sig -> Sig -> Sig -> Sig
mvclpf3 b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "mvclpf3" [(Ar,[Ar,Xr,Xr,Ir])] [a1,a2,a3]

-- | 
-- Moog voltage-controlled lowpass filter emulation.
--
-- Mvclpf4 is an digital implementation of the 4th-order (24 dB/oct) Moog ladder filter
-- originally written by Fons Andriaensen. It is a version of the
-- mvclpf3 opcode with four outputs, for 6dB, 12dB, 18dB, and
-- 24 dB/octave responses.
--
-- > asig1,asig2,asig3,asig4  mvclpf4  ain, xcf, xres[, istor]
--
-- csound doc: <http://csound.com/docs/manual/mvclpf4.html>
mvclpf4 ::  Sig -> Sig -> Sig -> (Sig,Sig,Sig,Sig)
mvclpf4 b1 b2 b3 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = mopcs "mvclpf4" ([Ar,Ar,Ar,Ar],[Ar,Xr,Xr,Ir]) [a1,a2,a3]

-- | 
-- A second-order resonant filter.
--
-- > ares  reson  asig, xcf, xbw [, iscl] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/reson.html>
reson ::  Sig -> Sig -> Sig -> Sig
reson b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "reson" [(Ar,[Ar,Xr,Xr,Ir,Ir])] [a1,a2,a3]

-- | 
-- A bandpass filter with variable frequency response.
--
-- Implementations of a second-order, two-pole two-zero bandpass filter with variable frequency response.
--
-- > ares  resonr  asig, xcf, xbw [, iscl] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/resonr.html>
resonr ::  Sig -> Sig -> Sig -> Sig
resonr b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "resonr" [(Ar,[Ar,Xr,Xr,Ir,Ir])] [a1,a2,a3]

-- | 
-- Emulates a stack of filters using the reson opcode.
--
-- resonx is equivalent to a filters consisting of more layers of reson with the same arguments, serially connected. Using a stack of a larger number of filters allows a sharper cutoff. They are faster than using a larger number instances in a Csound orchestra of the old opcodes, because only one initialization and k- cycle are needed at time and the audio loop falls entirely inside the cache memory of processor.
--
-- > ares  resonx  asig, xcf, xbw [, inumlayer] [, iscl] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/resonx.html>
resonx ::  Sig -> Sig -> Sig -> Sig
resonx b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "resonx" [(Ar,[Ar,Xr,Xr,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- A bank of second-order bandpass filters, connected in parallel.
--
-- > ares  resony  asig, kbf, kbw, inum, ksep [, isepmode] [, iscl] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/resony.html>
resony ::  Sig -> Sig -> Sig -> D -> Sig -> Sig
resony b1 b2 b3 b4 b5 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4 <*> unSig b5
    where f a1 a2 a3 a4 a5 = opcs "resony" [(Ar,[Ar,Kr,Kr,Ir,Kr,Ir,Ir,Ir])] [a1,a2,a3,a4,a5]

-- | 
-- A bandpass filter with variable frequency response.
--
-- Implementations of a second-order, two-pole two-zero bandpass filter with variable frequency response.
--
-- > ares  resonz  asig, xcf, xbw [, iscl] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/resonz.html>
resonz ::  Sig -> Sig -> Sig -> Sig
resonz b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "resonz" [(Ar,[Ar,Xr,Xr,Ir,Ir])] [a1,a2,a3]

-- | 
-- A resonant low-pass filter.
--
-- > ares  rezzy  asig, xfco, xres [, imode, iskip]
--
-- csound doc: <http://csound.com/docs/manual/rezzy.html>
rezzy ::  Sig -> Sig -> Sig -> Sig
rezzy b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "rezzy" [(Ar,[Ar,Xr,Xr,Ir,Ir])] [a1,a2,a3]

-- | 
-- State-variable filter.
--
-- Statevar is a new digital implementation of the analogue state-variable filter. 
-- This filter has four simultaneous outputs: high-pass, low-pass,
-- band-pass and band-reject. This filter uses oversampling for sharper
-- resonance (default: 3 times oversampling). It includes a
-- resonance limiter that prevents the filter from getting unstable.
--
-- > ahp,alp,abp,abr  statevar  ain, xcf, xq [, iosamps, istor]
--
-- csound doc: <http://csound.com/docs/manual/statevar.html>
statevar ::  Sig -> Sig -> Sig -> (Sig,Sig,Sig,Sig)
statevar b1 b2 b3 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = mopcs "statevar" ([Ar,Ar,Ar,Ar],[Ar,Xr,Xr,Ir,Ir]) [a1,a2,a3]

-- | 
-- A resonant second order filter, with simultaneous lowpass, highpass and bandpass outputs.
--
-- Implementation of a resonant second order filter, with simultaneous lowpass, highpass and bandpass outputs.
--
-- > alow, ahigh, aband  svfilter   asig, kcf, kq [, iscl] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/svfilter.html>
svfilter ::  Sig -> Sig -> Sig -> (Sig,Sig,Sig)
svfilter b1 b2 b3 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = mopcs "svfilter" ([Ar,Ar,Ar],[Ar,Kr,Kr,Ir,Ir]) [a1,a2,a3]

-- | 
-- Models some of the filter characteristics of a Roland TB303 voltage-controlled filter.
--
-- This opcode attempts to model some of the filter characteristics of a Roland TB303 voltage-controlled filter. Euler's method is used to approximate the system, rather than traditional filter methods. Cutoff frequency, Q, and distortion are all coupled. Empirical methods were used to try to unentwine,  but frequency is only approximate as a result. Future fixes for some problems with this opcode may break existing orchestras relying on this version of tbvcf.
--
-- > ares  tbvcf  asig, xfco, xres, kdist, kasym [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/tbvcf.html>
tbvcf ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig
tbvcf b1 b2 b3 b4 b5 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5
    where f a1 a2 a3 a4 a5 = opcs "tbvcf" [(Ar,[Ar,Xr,Xr,Kr,Kr,Ir])] [a1,a2,a3,a4,a5]

-- | 
-- A bank of filters in which the cutoff frequency can be separated under user control.
--
-- A bank of filters in which the cutoff frequency can be separated under user control
--
-- > ares  vlowres  asig, kfco, kres, iord, ksep
--
-- csound doc: <http://csound.com/docs/manual/vlowres.html>
vlowres ::  Sig -> Sig -> Sig -> D -> Sig -> Sig
vlowres b1 b2 b3 b4 b5 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4 <*> unSig b5
    where f a1 a2 a3 a4 a5 = opcs "vlowres" [(Ar,[Ar,Kr,Kr,Ir,Kr])] [a1,a2,a3,a4,a5]

-- Standard Filters:Control.

-- | 
-- A notch filter whose transfer functions are the complements of the reson opcode.
--
-- > kres  aresonk  ksig, kcf, kbw [, iscl] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/aresonk.html>
aresonk ::  Sig -> Sig -> Sig -> Sig
aresonk b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "aresonk" [(Kr,[Kr,Kr,Kr,Ir,Ir])] [a1,a2,a3]

-- | 
-- A hi-pass filter whose transfer functions are the complements of the tonek opcode.
--
-- > kres  atonek  ksig, khp [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/atonek.html>
atonek ::  Sig -> Sig -> Sig
atonek b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "atonek" [(Kr,[Kr,Kr,Ir])] [a1,a2]

-- | 
-- Generate glissandos starting from a control signal.
--
-- > kres  lineto  ksig, ktime
--
-- csound doc: <http://csound.com/docs/manual/lineto.html>
lineto ::  Sig -> Sig -> Sig
lineto b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "lineto" [(Kr,[Kr,Kr])] [a1,a2]

-- | 
-- Applies portamento to a step-valued control signal.
--
-- > kres  port  ksig, ihtim [, isig]
--
-- csound doc: <http://csound.com/docs/manual/port.html>
port ::  Sig -> D -> Sig
port b1 b2 = Sig $ f <$> unSig b1 <*> unD b2
    where f a1 a2 = opcs "port" [(Kr,[Kr,Ir,Ir])] [a1,a2]

-- | 
-- Applies portamento to a step-valued control signal.
--
-- > kres  portk  ksig, khtim [, isig]
--
-- csound doc: <http://csound.com/docs/manual/portk.html>
portk ::  Sig -> Sig -> Sig
portk b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "portk" [(Kr,[Kr,Kr,Ir])] [a1,a2]

-- | 
-- A second-order resonant filter.
--
-- > kres  resonk  ksig, kcf, kbw [, iscl] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/resonk.html>
resonk ::  Sig -> Sig -> Sig -> Sig
resonk b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "resonk" [(Kr,[Kr,Kr,Kr,Ir,Ir])] [a1,a2,a3]

-- | 
-- Control signal resonant filter stack.
--
-- resonxk is equivalent to a group of resonk filters, with the same arguments, serially connected. Using a stack of a larger number of filters allows a sharper cutoff.
--
-- > kres  resonxk  ksig, kcf, kbw[, inumlayer, iscl, istor]
--
-- csound doc: <http://csound.com/docs/manual/resonxk.html>
resonxk ::  Sig -> Sig -> Sig -> Sig
resonxk b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "resonxk" [(Kr,[Kr,Kr,Kr,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Exponential Lag
--
-- Exponential lag with 60dB lag time. Port of Supercollider's Lag
--
-- > aout  sc_lag  ain, klagtime [, initialvalue=0]
-- > kout  sc_lag  kin, klagtime [, initialvalue=0]
--
-- csound doc: <http://csound.com/docs/manual/sc_lag.html>
sc_lag ::  Sig -> Sig -> Sig
sc_lag b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "sc_lag" [(Ar,[Ar,Kr,Ir]),(Kr,[Kr,Kr,Ir])] [a1,a2]

-- | 
-- Exponential Lag
--
-- Exponential lag with different smoothing time for up- and
-- 	  downgoing signals. Port of Supercollider's LagUD
--
-- > aout  sc_lagud  ain, klagup, klagdown
-- > kout  sc_lagud  kin, klagup, klagdown
--
-- csound doc: <http://csound.com/docs/manual/sc_lagud.html>
sc_lagud ::  Sig -> Sig -> Sig -> Sig
sc_lagud b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "sc_lagud" [(Ar,[Ar,Kr,Kr]),(Kr,[Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Timed trigger
--
-- Timed trigger. Port of Supercollider's Trig ugen
--
-- > aout  sc_trig  ain, kdur
-- > kout  sc_trig  kin, kdur
--
-- csound doc: <http://csound.com/docs/manual/sc_trig.html>
sc_trig ::  Sig -> Sig -> Sig
sc_trig b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "sc_trig" [(Ar,[Ar,Kr]),(Kr,[Kr,Kr])] [a1,a2]

-- | 
-- Generate glissandos starting from a control signal.
--
-- Generate glissandos starting from a control signal with a trigger.
--
-- > kres  tlineto  ksig, ktime, ktrig
--
-- csound doc: <http://csound.com/docs/manual/tlineto.html>
tlineto ::  Sig -> Sig -> Sig -> Sig
tlineto b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "tlineto" [(Kr,[Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- A first-order recursive low-pass filter with variable frequency response.
--
-- > kres  tonek  ksig, khp [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/tonek.html>
tonek ::  Sig -> Sig -> Sig
tonek b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "tonek" [(Kr,[Kr,Kr,Ir])] [a1,a2]

-- Specialized Filters.

-- | 
-- A DC blocking filter.
--
-- Implements the DC blocking filter
--
-- > ares  dcblock  ain [, igain]
--
-- csound doc: <http://csound.com/docs/manual/dcblock.html>
dcblock ::  Sig -> Sig
dcblock b1 = Sig $ f <$> unSig b1
    where f a1 = opcs "dcblock" [(Ar,[Ar,Ir])] [a1]

-- | 
-- A DC blocking filter.
--
-- Implements a DC blocking filter with improved DC attenuation.
--
-- > ares  dcblock2  ain [, iorder] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/dcblock2.html>
dcblock2 ::  Sig -> Sig
dcblock2 b1 = Sig $ f <$> unSig b1
    where f a1 = opcs "dcblock2" [(Ar,[Ar,Ir,Ir])] [a1]

-- | 
-- Equalizer filter
--
-- The opcode eqfil is a 2nd order tunable equalisation filter based on Regalia and Mitra
--       design ("Tunable Digital Frequency Response Equalization Filters", IEEE Trans.
--       on Ac., Sp. and Sig Proc., 35 (1), 1987). It provides a peak/notch filter for
--       building parametric/graphic equalisers.
--
-- > asig  eqfil  ain, kcf, kbw, kgain[, istor]
--
-- csound doc: <http://csound.com/docs/manual/eqfil.html>
eqfil ::  Sig -> Sig -> Sig -> Sig -> Sig
eqfil b1 b2 b3 b4 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
    where f a1 a2 a3 a4 = opcs "eqfil" [(Ar,[Ar,Kr,Kr,Kr,Ir])] [a1,a2,a3,a4]

-- | 
-- Performs filtering using a transposed form-II digital filter lattice with no time-varying control.
--
-- General purpose custom filter with no time-varying pole control. The filter coefficients implement the following difference equation:
--
-- > ares  filter2  asig, iM, iN, ib0, ib1, ..., ibM, ia1, ia2, ..., iaN
-- > kres  filter2  ksig, iM, iN, ib0, ib1, ..., ibM, ia1, ia2, ..., iaN
--
-- csound doc: <http://csound.com/docs/manual/filter2.html>
filter2 ::  Sig -> D -> D -> [D] -> Sig
filter2 b1 b2 b3 b4 = Sig $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> mapM unD b4
    where f a1 a2 a3 a4 = opcs "filter2" [(Ar,[Ar] ++ (repeat Ir)),(Kr,[Kr] ++ (repeat Ir))] ([a1
                                                                                              ,a2
                                                                                              ,a3] ++ a4)

-- | 
-- AM/FM analysis from quadrature signal.
--
-- This opcode attempts to extract the AM and FM signals off a
--       quadrature signal (e.g. from a Hilbert transform).
--
-- > am, af  fmanal  are, aim
--
-- csound doc: <http://csound.com/docs/manual/fmanal.html>
fmanal ::  Sig -> Sig -> (Sig,Sig)
fmanal b1 b2 = pureTuple $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = mopcs "fmanal" ([Ar,Ar],[Ar,Ar]) [a1,a2]

-- | 
-- Formant filter.
--
-- Fofilter generates a stream of overlapping sinewave grains, when fed with 
-- a pulse train. Each grain is the impulse response of a combination of 
-- two BP filters. The grains are defined by their attack time (determining 
-- the skirtwidth of the formant region at -60dB) and decay time 
-- (-6dB bandwidth). Overlapping will occur when 1/freq < decay, but, 
-- unlike FOF, there is no upper limit on the number of overlaps. 
-- The original idea for this opcode came from J McCartney's formlet class 
-- in SuperCollider, but this is possibly implemented differently(?).
--
-- > asig  fofilter  ain, xcf, xris, xdec[, istor]
--
-- csound doc: <http://csound.com/docs/manual/fofilter.html>
fofilter ::  Sig -> Sig -> Sig -> Sig -> Sig
fofilter b1 b2 b3 b4 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
    where f a1 a2 a3 a4 = opcs "fofilter" [(Ar,[Ar,Xr,Xr,Xr,Ir])] [a1,a2,a3,a4]

-- | 
-- A Hilbert transformer.
--
-- An IIR implementation of a Hilbert transformer.
--
-- > ar1, ar2  hilbert  asig
--
-- csound doc: <http://csound.com/docs/manual/hilbert.html>
hilbert ::  Sig -> (Sig,Sig)
hilbert b1 = pureTuple $ f <$> unSig b1
    where f a1 = mopcs "hilbert" ([Ar,Ar],[Ar]) [a1]

-- | 
-- A Hilbert rransformer.
--
-- A DFT-based implementation of a Hilbert transformer.
--
-- > ar1, ar2  hilbert2  asig, ifftsize, ihopsize
--
-- csound doc: <http://csound.com/docs/manual/hilbert2.html>
hilbert2 ::  Sig -> D -> D -> (Sig,Sig)
hilbert2 b1 b2 b3 = pureTuple $ f <$> unSig b1 <*> unD b2 <*> unD b3
    where f a1 a2 a3 = mopcs "hilbert2" ([Ar,Ar],[Ar,Ir,Ir]) [a1,a2,a3]

-- | 
-- A filter with a non-linear effect.
--
-- Implements the filter:
--
-- > ares  nlfilt  ain, ka, kb, kd, kC, kL
--
-- csound doc: <http://csound.com/docs/manual/nlfilt.html>
nlfilt ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
nlfilt b1 b2 b3 b4 b5 b6 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6
    where f a1 a2 a3 a4 a5 a6 = opcs "nlfilt" [(Ar,[Ar,Kr,Kr,Kr,Kr,Kr])] [a1,a2,a3,a4,a5,a6]

-- | 
-- A filter with a non-linear effect and blowup protection.
--
-- Implements the filter:
--
-- > ares  nlfilt2  ain, ka, kb, kd, kC, kL
--
-- csound doc: <http://csound.com/docs/manual/nlfilt2.html>
nlfilt2 ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
nlfilt2 b1 b2 b3 b4 b5 b6 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6
    where f a1 a2 a3 a4 a5 a6 = opcs "nlfilt2" [(Ar,[Ar,Kr,Kr,Kr,Kr,Kr])] [a1,a2,a3,a4,a5,a6]

-- | 
-- Implementation of Zoelzer's parametric equalizer filters.
--
-- Implementation of Zoelzer's parametric equalizer filters, with some modifications by the author.
--
-- > ares  pareq  asig, kc, kv, kq [, imode] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/pareq.html>
pareq ::  Sig -> Sig -> Sig -> Sig -> Sig
pareq b1 b2 b3 b4 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
    where f a1 a2 a3 a4 = opcs "pareq" [(Ar,[Ar,Kr,Kr,Kr,Ir,Ir])] [a1,a2,a3,a4]

-- | 
-- Parametric equalizer and filter opcode with 7 filter types, based
--       on algorithm by Robert Bristow-Johnson.
--
-- Parametric equalizer and filter opcode with 7 filter types,
--     	based on algorithm by Robert Bristow-Johnson.
--
-- > ar  rbjeq  asig, kfco, klvl, kQ, kS[, imode]
--
-- csound doc: <http://csound.com/docs/manual/rbjeq.html>
rbjeq ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig
rbjeq b1 b2 b3 b4 b5 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5
    where f a1 a2 a3 a4 a5 = opcs "rbjeq" [(Ar,[Ar,Kr,Kr,Kr,Kr,Ir])] [a1,a2,a3,a4,a5]

-- | 
-- Performs filtering using a transposed form-II digital filter lattice with radial pole-shearing and angular pole-warping.
--
-- General purpose custom filter with time-varying pole control. The filter coefficients implement the following difference equation:
--
-- > ares  zfilter2  asig, kdamp, kfreq, iM, iN, ib0, ib1, ..., ibM, \
-- >           ia1,ia2, ..., iaN
--
-- csound doc: <http://csound.com/docs/manual/zfilter2.html>
zfilter2 ::  Sig -> Sig -> Sig -> D -> D -> [D] -> Sig
zfilter2 b1 b2 b3 b4 b5 b6 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4 <*> unD b5 <*> mapM unD b6
    where f a1 a2 a3 a4 a5 a6 = opcs "zfilter2" [(Ar,[Ar,Kr,Kr] ++ (repeat Ir))] ([a1
                                                                                  ,a2
                                                                                  ,a3
                                                                                  ,a4
                                                                                  ,a5] ++ a6)

-- Waveguides.

-- | 
-- A simple waveguide model consisting of one delay-line and one first-order lowpass filter.
--
-- > ares  wguide1  asig, xfreq, kcutoff, kfeedback
--
-- csound doc: <http://csound.com/docs/manual/wguide1.html>
wguide1 ::  Sig -> Sig -> Sig -> Sig -> Sig
wguide1 b1 b2 b3 b4 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
    where f a1 a2 a3 a4 = opcs "wguide1" [(Ar,[Ar,Xr,Kr,Kr])] [a1,a2,a3,a4]

-- | 
-- A model of beaten plate consisting of two parallel delay-lines and two first-order lowpass filters.
--
-- > ares  wguide2  asig, xfreq1, xfreq2, kcutoff1, kcutoff2, \
-- >           kfeedback1, kfeedback2
--
-- csound doc: <http://csound.com/docs/manual/wguide2.html>
wguide2 ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
wguide2 b1 b2 b3 b4 b5 b6 b7 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7
    where f a1 a2 a3 a4 a5 a6 a7 = opcs "wguide2" [(Ar,[Ar,Xr,Xr,Kr,Kr,Kr,Kr])] [a1
                                                                                ,a2
                                                                                ,a3
                                                                                ,a4
                                                                                ,a5
                                                                                ,a6
                                                                                ,a7]

-- Waveshaping.

-- | 
-- Efficiently evaluates the sum of Chebyshev polynomials of arbitrary order.
--
-- The chebyshevpoly opcode calculates the value of a polynomial expression with a single a-rate input variable that is made up of a linear combination of the first N Chebyshev polynomials of the first kind.  Each Chebyshev polynomial, Tn(x), is weighted by a k-rate coefficient, kn, so that the opcode is calculating a sum of any number of terms in the form kn*Tn(x).  Thus, the chebyshevpoly opcode allows for the waveshaping of an audio signal with a dynamic transfer function that gives precise control over the harmonic content of the output.
--
-- > aout  chebyshevpoly  ain, k0 [, k1 [, k2 [...]]]
--
-- csound doc: <http://csound.com/docs/manual/chebyshevpoly.html>
chebyshevpoly ::  Sig -> [Sig] -> Sig
chebyshevpoly b1 b2 = Sig $ f <$> unSig b1 <*> mapM unSig b2
    where f a1 a2 = opcs "chebyshevpoly" [(Ar,[Ar] ++ (repeat Kr))] ([a1] ++ a2)

-- | 
-- Performs linear clipping on an audio signal or a phasor.
--
-- The pdclip opcode allows a percentage of the input range of a signal to be clipped to fullscale. It is similar to simply multiplying the signal and limiting the range of the result, but pdclip allows you to think about how much of the signal range is being distorted instead of the scalar factor and has a offset parameter for assymetric clipping of the signal range. pdclip is also useful for remapping phasors for phase distortion synthesis.
--
-- > aout  pdclip  ain, kWidth, kCenter [, ibipolar [, ifullscale]]
--
-- csound doc: <http://csound.com/docs/manual/pdclip.html>
pdclip ::  Sig -> Sig -> Sig -> Sig
pdclip b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "pdclip" [(Ar,[Ar,Kr,Kr,Ir,Ir])] [a1,a2,a3]

-- | 
-- Distorts a phasor for reading the two halves of a table at different rates.
--
-- The pdhalf opcode is designed to emulate the "classic" phase distortion synthesis method of the Casio CZ-series of synthesizers from the mid-1980's.  This technique reads the first and second halves of a function table at different rates in order to warp the waveform.  For example, pdhalf can smoothly transform a sine wave into something approximating the shape of a saw wave.
--
-- > aout  pdhalf  ain, kShapeAmount [, ibipolar [, ifullscale]]
--
-- csound doc: <http://csound.com/docs/manual/pdhalf.html>
pdhalf ::  Sig -> Sig -> Sig
pdhalf b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "pdhalf" [(Ar,[Ar,Kr,Ir,Ir])] [a1,a2]

-- | 
-- Distorts a phasor for reading two unequal portions of a table in equal periods.
--
-- The pdhalfy opcode is a variation on the phase distortion synthesis method of the pdhalf opcode.  It is useful for distorting a phasor in order to read two unequal portions of a table in the same number of samples.
--
-- > aout  pdhalfy  ain, kShapeAmount [, ibipolar [, ifullscale]]
--
-- csound doc: <http://csound.com/docs/manual/pdhalfy.html>
pdhalfy ::  Sig -> Sig -> Sig
pdhalfy b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "pdhalfy" [(Ar,[Ar,Kr,Ir,Ir])] [a1,a2]

-- | 
-- Waveshapes a signal by raising it to a variable exponent.
--
-- The powershape opcode raises an input signal to a power with pre- and post-scaling of the signal so that the output will be in a predictable range.  It also processes negative inputs in a symmetrical way to positive inputs, calculating a dynamic transfer function that is useful for waveshaping.
--
-- > aout  powershape  ain, kShapeAmount [, ifullscale]
--
-- csound doc: <http://csound.com/docs/manual/powershape.html>
powershape ::  Sig -> Sig -> Sig
powershape b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "powershape" [(Ar,[Ar,Kr,Ir])] [a1,a2]

-- Comparators and Accumulators.

-- | 
-- Compares two audio signals
--
-- Compares two audio signals using the standard math operators
--
-- > aout  cmp  aL, S_operator, aR
--
-- csound doc: <http://csound.com/docs/manual/cmp.html>
cmp ::  Sig -> Str -> Sig -> Sig
cmp b1 b2 b3 = Sig $ f <$> unSig b1 <*> unStr b2 <*> unSig b3
    where f a1 a2 a3 = opcs "cmp" [(Ar,[Ar,Sr,Ar])] [a1,a2,a3]

-- | 
-- Produces a signal that is the maximum of any number of input signals.
--
-- The max opcode takes any number of a-rate,
--       k-rate or i-rate signals as input (all of the same rate), and outputs a signal at the same rate that is the maximum of all of the inputs.  For a-rate signals, the inputs are compared one sample at a time (i.e. max does not scan an entire ksmps period of a signal for its local maximum as the max_k opcode does).
--
-- > amax  max  ain1, ain2 [, ain3] [, ain4] [...]
-- > kmax  max  kin1, kin2 [, kin3] [, kin4] [...]
-- > imax  max  iin1, iin2 [, iin3] [, iin4] [...]
--
-- csound doc: <http://csound.com/docs/manual/max.html>
max' ::  [Sig] -> Sig
max' b1 = Sig $ f <$> mapM unSig b1
    where f a1 = opcs "max" [(Ar,(repeat Ar)),(Kr,(repeat Kr)),(Ir,(repeat Ir))] a1

-- | 
-- Local maximum (or minimum) value of an incoming asig signal
--
-- max_k outputs the local maximum (or minimum) value of  the incoming asig signal, checked in the time interval between ktrig has become true twice.
--
-- > knumkout  max_k  asig, ktrig, itype
--
-- csound doc: <http://csound.com/docs/manual/max_k.html>
max_k ::  Sig -> Sig -> D -> Sig
max_k b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3
    where f a1 a2 a3 = opcs "max_k" [(Kr,[Ar,Kr,Ir])] [a1,a2,a3]

-- | 
-- Produces a signal that is the maximum of the absolute values of any number of input signals.
--
-- The maxabs opcode takes any number of a-rate or k-rate signals as input (all of the same rate), and outputs a signal at the same rate that is the maximum of all of the inputs.  It is identical to the max opcode except that it takes the absolute value of each input before comparing them.  Therefore, the output is always non-negative.  For a-rate signals, the inputs are compared one sample at a time (i.e. maxabs does not scan an entire ksmps period of a signal for its local maximum as the max_k opcode does).
--
-- > amax  maxabs  ain1, ain2 [, ain3] [, ain4] [...]
-- > kmax  maxabs  kin1, kin2 [, kin3] [, kin4] [...]
--
-- csound doc: <http://csound.com/docs/manual/maxabs.html>
maxabs ::  [Sig] -> Sig
maxabs b1 = Sig $ f <$> mapM unSig b1
    where f a1 = opcs "maxabs" [(Ar,(repeat Ar)),(Kr,(repeat Kr))] a1

-- | 
-- Accumulates the maximum of the absolute values of audio signals.
--
-- maxabsaccum compares two audio-rate variables and stores the maximum of their absolute values into the first.
--
-- >  maxabsaccum  aAccumulator, aInput
--
-- csound doc: <http://csound.com/docs/manual/maxabsaccum.html>
maxabsaccum ::  Sig -> Sig -> SE ()
maxabsaccum b1 b2 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "maxabsaccum" [(Xr,[Ar,Ar])] [a1,a2]

-- | 
-- Accumulates the maximum value of audio signals.
--
-- maxaccum compares two audio-rate variables and stores the maximum value between them into the first.
--
-- >  maxaccum  aAccumulator, aInput
--
-- csound doc: <http://csound.com/docs/manual/maxaccum.html>
maxaccum ::  Sig -> Sig -> SE ()
maxaccum b1 b2 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "maxaccum" [(Xr,[Ar,Ar])] [a1,a2]

-- | 
-- Produces a signal that is the minimum of any number of input signals.
--
-- The min opcode takes any number of a-rate,
--       k-rate or i-rate signals as input (all of the same rate), and outputs a signal at the same rate that is the minimum of all of the inputs.  For a-rate signals, the inputs are compared one sample at a time (i.e. min does not scan an entire ksmps period of a signal for its local minimum as the max_k opcode does).
--
-- > amin  min  ain1, ain2 [, ain3] [, ain4] [...]
-- > kmin  min  kin1, kin2 [, kin3] [, kin4] [...]
-- > imin  min  iin1, iin2 [, iin3] [, iin4] [...]
--
-- csound doc: <http://csound.com/docs/manual/min.html>
min' ::  [Sig] -> Sig
min' b1 = Sig $ f <$> mapM unSig b1
    where f a1 = opcs "min" [(Ar,(repeat Ar)),(Kr,(repeat Kr)),(Ir,(repeat Ir))] a1

-- | 
-- Produces a signal that is the minimum of the absolute values of any number of input signals.
--
-- The minabs opcode takes any number of a-rate or k-rate signals as input (all of the same rate), and outputs a signal at the same rate that is the minimum of all of the inputs.  It is identical to the min opcode except that it takes the absolute value of each input before comparing them.  Therefore, the output is always non-negative.    For a-rate signals, the inputs are compared one sample at a time (i.e. minabs does not scan an entire ksmps period of a signal for its local minimum as the max_k opcode does).
--
-- > amin  minabs  ain1, ain2 [, ain3] [, ain4] [...]
-- > kmin  minabs  kin1, kin2 [, kin3] [, kin4] [...]
--
-- csound doc: <http://csound.com/docs/manual/minabs.html>
minabs ::  [Sig] -> Sig
minabs b1 = Sig $ f <$> mapM unSig b1
    where f a1 = opcs "minabs" [(Ar,(repeat Ar)),(Kr,(repeat Kr))] a1

-- | 
-- Accumulates the minimum of the absolute values of audio signals.
--
-- minabsaccum compares two audio-rate variables and stores the minimum of their absolute values into the first.
--
-- >  minabsaccum  aAccumulator, aInput
--
-- csound doc: <http://csound.com/docs/manual/minabsaccum.html>
minabsaccum ::  Sig -> Sig -> SE ()
minabsaccum b1 b2 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "minabsaccum" [(Xr,[Ar,Ar])] [a1,a2]

-- | 
-- Accumulates the minimum value of audio signals.
--
-- minaccum compares two audio-rate variables and stores the minimum value between them into the first.
--
-- >  minaccum  aAccumulator, aInput
--
-- csound doc: <http://csound.com/docs/manual/minaccum.html>
minaccum ::  Sig -> Sig -> SE ()
minaccum b1 b2 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "minaccum" [(Xr,[Ar,Ar])] [a1,a2]