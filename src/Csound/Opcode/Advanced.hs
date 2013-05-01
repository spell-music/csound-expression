-- | Advanced Signal Processing
module Csound.Opcode.Advanced (
    -----------------------------------------------------
    -- * Modulation and Distortion

    -- ** Frequency Modulation
    foscil, foscili,
    crossfm, crossfmi, crosspm, crosspmi, crossfmpm, crossfmpmi,

    -- ** Distortion and Wave Shaping
    distort, distort1, powershape, polynomial, chebyshevpoly,  

    -- ** Flanging, Phasing, Phase Shaping
    flanger, harmon, phaser1, phaser2, pdclip, pdhalf, pdhalfy,

    -- ** Doppler Shift
    doppler,
    

    -----------------------------------------------------
    -- * Granular Synthesis
    fof,
    
    -----------------------------------------------------
    -- * Convolution
    pconvolve, convolve, ftconv, dconv,  

    -----------------------------------------------------
    -- * FFT and Spectral Processing

    -- ** Realtime Analysis And Resynthesis
    pvsanal, pvstanal, pvsynth, pvsadsyn,

    -- ** Writing FFT Data To A File And Reading From It
    pvswrite, pvsfread, pvsdiskin,
    
    -- ** FFT Info
    pvsinfo, pvsbin, pvscent,  

    -- ** Manipulating FFT Signals
    pvscale, pvshift, pvsbandp, pvsbandr, pvsmix, pvscross, pvsfilter,
    pvsvoc, pvsmorph, pvsfreeze, pvsmaska, pvsblur, pvstencil, pvsarp, pvsmooth,
   
    -----------------------------------------------------
    -- * Physical Models and FM Instruments

    -- ** Waveguide Physical Modelling
    streson, pluck, repluck, wgbow, wgbowedbar, wgbrass,
    wgclar, wgflute, wgpluck, wgpluck2, wguide1, wguide2,

    -- ** FM Instrument Models
    fmb3, fmbell, fmmetal, fmpercfl, fmrhode, fmvoice, fmwurlie,
    
    -- ** PhISEM opcodes
    -- | PhISEM (Physically Informed Stochastic Event Modeling) is an algorithmic approach for simulating collisions of multiple independent sound producing objects. 
    bamboo, cabasa, crunch, dripwater, guiro, sandpaper, sekere, sleighbells, stix, tambourine,
    
    -- ** Some Perry Cook's instruments
    -- | The method is a physically inspired model developed from Perry Cook, but re-coded for Csound.  
    gogobel, marimba, shaker, vibes,
    
    -- ** Other Models      
    barmodel, mandol, moog, voice
) where

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.Tuple
import Csound.LowLevel

-----------------------------------------------------
-- * Modulation and Distortion

-----------------------------------------------------
-- ** Frequency Modulation

-- | A basic frequency modulated oscillator. 
--
-- > ares foscil xamp, kcps, xcar, xmod, kndx, ifn [, iphs]
--
-- doc: <http://www.csounds.com/manual/html/foscil.html>
foscil :: Amp -> Cps -> Sig -> Sig -> Sig -> Tab -> Sig
foscil = opc6 "foscil" [(a, [x, k, x, x, k, i, i])]

-- | Basic frequency modulated oscillator with linear interpolation. 
--
-- > ares foscili xamp, kcps, xcar, xmod, kndx, ifn [, iphs]
--
-- doc: <http://www.csounds.com/manual/html/foscili.html>
foscili :: Amp -> Cps -> Sig -> Sig -> Sig -> Tab -> Sig
foscili = opc6 "foscili" [(a, [x, k, x, x, k, i, i])]


-- | Two oscillators, mutually frequency and/or phase modulated by each other. 
--
-- > a1, a2 crossfm xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
-- > a1, a2 crossfmi xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
-- > a1, a2 crosspm xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
-- > a1, a2 crosspmi xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
-- > a1, a2 crossfmpm xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
-- > a1, a2 crossfmpmi xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
--
-- doc: <http://www.csounds.com/manual/html/crossfm.html> 
crossfmGen :: Name -> Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> (Sig, Sig)
crossfmGen name = mopc7 name ([a, a], [x, x, x, x, k, i, i, i, i])

crossfm, crossfmi, crosspm, crosspmi, crossfmpm, crossfmpmi :: Cps -> Cps -> Sig -> Sig -> Sig -> Tab -> Tab -> (Sig, Sig)

crossfm = crossfmGen "crossfm"
crossfmi = crossfmGen "crossfmi"
crosspm = crossfmGen "crosspm"
crosspmi = crossfmGen "crosspmi"
crossfmpm = crossfmGen "crossfmpm"
crossfmpmi = crossfmGen "crossfmpmi"

-----------------------------------------------------
-- ** Distortion and Wave Shaping

-- | Distort an audio signal via waveshaping and optional clipping. 
--
-- > ar distort asig, kdist, ifn[, ihp, istor]
--
-- doc: <http://www.csounds.com/manual/html/distort.html>
distort :: Sig -> Sig -> Tab -> Sig
distort = opc3 "distort" [(a, [a, k, i, i, i])]

-- | Implementation of modified hyperbolic tangent distortion. distort1 can be used to generate wave shaping distortion based on a modification of the tanh function.
--
-- >          exp(asig * (shape1 + pregain)) - exp(asig * (shape2 - pregain))
-- >   aout = ---------------------------------------------------------------
-- >          exp(asig * pregain)            + exp(-asig * pregain)
--
-- > ares distort1 asig, kpregain, kpostgain, kshape1, kshape2[, imode]
--
-- doc: <http://www.csounds.com/manual/html/distort1.html>
distort1 :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
distort1 = opc5 "distort1" [(a, [a, k, k, k, k, i])]

-- | The powershape opcode raises an input signal to a power with pre- and post-scaling of the signal so that 
-- the output will be in a predictable range. It also processes negative inputs in a symmetrical way to positive inputs, calculating a dynamic transfer function that is useful for waveshaping. 
--
-- > aout powershape ain, kShapeAmount [, ifullscale]
--
-- doc: <http://www.csounds.com/manual/html/powershape.html>
powershape :: Sig -> Sig -> Sig
powershape = opc2 "powershape" [(a, [a, k, i])]

-- | The polynomial opcode calculates a polynomial with a single a-rate input variable. The polynomial is a sum 
-- of any number of terms in the form kn*x^n where kn is the nth coefficient of the expression. These coefficients are k-rate values. 
--
-- > aout polynomial ain, k0 [, k1 [, k2 [...]]]
--
-- doc: <http://www.csounds.com/manual/html/polynomial.html>
polynomial :: Sig -> [Sig] -> Sig
polynomial a1 a2 = opcs "polynomial" [(a, a:repeat k)] (a1:a2)

-- | The chebyshevpoly opcode calculates the value of a polynomial expression with a single a-rate input variable that is 
-- made up of a linear combination of the first N Chebyshev polynomials of the first kind. Each Chebyshev polynomial, Tn(x), 
-- is weighted by a k-rate coefficient, kn, so that the opcode is calculating a sum of any number of terms in the form kn*Tn(x). 
-- Thus, the chebyshevpoly opcode allows for the waveshaping of an audio signal with a dynamic transfer function that gives 
-- precise control over the harmonic content of the output. 
--
-- > aout chebyshevpoly ain, k0 [, k1 [, k2 [...]]]
--
-- doc: <http://www.csounds.com/manual/html/chebyshevpoly.html>
chebyshevpoly :: Sig -> [Sig] -> Sig
chebyshevpoly a1 a2 = opcs "chebyshevpoly" [(a, a:repeat k)] (a1:a2)

-----------------------------------------------------
-- ** Flanging, Phasing, Phase Shaping

-- | A user controlled flanger. 
--
-- > ares flanger asig, adel, kfeedback [, imaxd]
--
-- doc: <http://www.csounds.com/manual/html/flanger.html>
flanger :: Sig -> Sig -> Sig -> Sig
flanger = opc3 "flanger" [(a, [a, a, k, i])]

-- | Analyze an audio input and generate harmonizing voices in synchrony. 
--
-- > ares harmon asig, kestfrq, kmaxvar, kgenfreq1, kgenfreq2, imode, \
-- >       iminfrq, iprd
--
-- doc: <http://www.csounds.com/manual/html/harmon.html>
harmon :: Sig -> Sig -> Sig -> Sig -> Sig -> D -> D -> D -> Sig  
harmon = opc8 "harmon" [(a, [a, k, k, k, k, i, i, i])]

-- | An implementation of iord number of first-order allpass filters in series. 
--
-- > ares phaser1 asig, kfreq, kord, kfeedback [, iskip]
--
-- doc: <http://www.csounds.com/manual/html/phaser1.html>
phaser1 :: Sig -> Sig -> Sig -> Sig -> Sig
phaser1 = opc4 "phaser1" [(a, [a, k, k, k, i])]

-- | An implementation of iord number of second-order allpass filters in series. 
--
-- > ares phaser2 asig, kfreq, kq, kord, kmode, ksep, kfeedback
--
-- doc: <http://www.csounds.com/manual/html/phaser2.html>
phaser2 :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
phaser2 = opc7 "phaser2" [(a, [a, k, k, k, k, k, k])]

-- | The pdclip opcode allows a percentage of the input range of a signal to be clipped to fullscale. 
-- It is similar to simply multiplying the signal and limiting the range of the result, but pdclip 
-- allows you to think about how much of the signal range is being distorted instead of the scalar 
-- factor and has a offset parameter for assymetric clipping of the signal range. pdclip is also useful
-- for remapping phasors for phase distortion synthesis. 
--
-- > aout pdclip ain, kWidth, kCenter [, ibipolar [, ifullscale]]
--
-- doc: <http://www.csounds.com/manual/html/pdclip.html>
pdclip :: Sig -> Sig -> Sig -> Sig
pdclip = opc3 "pdclip" [(a, [a, k, k, i, i])]

-- | The pdhalf opcode is designed to emulate the "classic" phase distortion synthesis method of the 
-- Casio CZ-series of synthesizers from the mid-1980's. This technique reads the first and second halves 
-- of a function table at different rates in order to warp the waveform. For example, pdhalf can smoothly 
-- transform a sine wave into something approximating the shape of a saw wave. 
--
-- > aout pdhalf ain, kShapeAmount [, ibipolar [, ifullscale]]
--
-- doc: <http://www.csounds.com/manual/html/pdhalf.html>
pdhalf :: Sig -> Sig -> Sig -> Sig
pdhalf = opc3 "pdhalf" [(a, [a, k, k, i, i])]

-- | The pdhalfy opcode is a variation on the phase distortion synthesis method of the pdhalf opcode. 
-- It is useful for distorting a phasor in order to read two unequal portions of a table in the same number of samples. 
--
-- > aout pdhalfy ain, kShapeAmount [, ibipolar [, ifullscale]]
--
-- doc: <http://www.csounds.com/manual/html/pdhalfy.html>
pdhalfy :: Sig -> Sig -> Sig -> Sig
pdhalfy = opc3 "pdhalfy" [(a, [a, k, k, i, i])]

-----------------------------------------------------
-- ** Doppler Shift

-- | A fast and robust method for approximating sound propagation, achieving convincing 
-- Doppler shifts without having to solve equations. The method computes frequency shifts 
-- based on reading an input delay line at a delay time computed from the distance between 
-- source and mic and the speed of sound. One instance of the opcode is required for each 
-- dimension of space through which the sound source moves. If the source sound moves at 
-- a constant speed from in front of the microphone, through the microphone, to behind 
-- the microphone, then the output will be frequency shifted above the source frequency 
-- at a constant frequency while the source approaches, then discontinuously will be 
-- shifted below the source frequency at a constant frequency as the source recedes from 
-- the microphone. If the source sound moves at a constant speed through a point to one 
-- side of the microphone, then the rate of change of position will not be constant, and 
-- the familiar Doppler frequency shift typical of a siren or engine approaching and 
-- receding along a road beside a listener will be heard. 
--
-- > ashifted doppler asource, ksourceposition, kmicposition [, isoundspeed, ifiltercutoff]
--
-- doc: <http://www.csounds.com/manual/html/doppler.html>
doppler :: Sig -> Sig -> Sig -> Sig
doppler = opc3 "doppler" [(a, [a, k, k, i, i])]

-----------------------------------------------------
-- * Granular Synthesis

-- | Audio output is a succession of sinusoid bursts initiated at frequency
-- xfund with a spectral peak at xform. For xfund above 25 Hz these burts produce 
-- a speech-like formant with spectral characteristics determined by the k-input parameters.
-- For lower fundamentals this generator provides a special form of granular synthesis.
--
-- > ar fof xamp, xfund, xform, koct, kband, kris, kdur, kdec, iolaps, ifna, ifnb, itotdur, [iphs, ifmode]
--
-- doc: <http://www.csounds.com/manual/html/fof.html>
fof :: Amp -> Cps -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> Tab -> Tab -> D -> Sig
fof = opc12 "fof" [(a, [x, x, x, k, k, k, k, k, i, i, i, i, i, i, i])]   

-----------------------------------------------------
-- * Convolution

-- | Convolution based on a uniformly partitioned overlap-save algorithm. Compared to the convolve opcode, pconvolve has these benefits:
--
--    * small delay
--
--    * possible to run in real-time for shorter impulse files
--
--    * no pre-process analysis pass
--
--    * can often render faster than convolve
--
--
-- > ar1 [, ar2] [, ar3] [, ar4] pconvolve ain, ifilcod [, ipartitionsize, ichannel]
--
-- doc: <http://www.csounds.com/manual/html/pconvolve.html>
pconvolve :: CsdTuple a => Sig -> Str -> a
pconvolve = mopc2 "pconvolve" ([a,a,a,a], [a,s,i,i])

-- | Output is the convolution of signal ain and the impulse response contained in ifilcod. If more than one 
-- output signal is supplied, each will be convolved with the same impulse response. Note that it is 
-- considerably more efficient to use one instance of the operator when processing a mono input to create stereo, or quad, outputs. 
--
-- > ar1 [, ar2] [, ar3] [, ar4] convolve ain, ifilcod [, ichannel]
--
-- doc: <http://www.csounds.com/manual/html/convolve.html>
convolve :: CsdTuple a => Sig -> D -> a
convolve = mopc2 "convolve" ([a, a, a, a], [a, i, i])

-- | Low latency multichannel convolution, using a function table as impulse response source. The algorithm is to split the 
-- impulse response to partitions of length determined by the iplen parameter, and delay and mix partitions so that the original, 
-- full length impulse response is reconstructed without gaps. The output delay (latency) is iplen samples, and does not depend 
-- on the control rate, unlike in the case of other convolve opcodes. 
--
-- > a1[, a2[, a3[, ... a8]]] ftconv ain, ift, iplen[, iskipsamples \
-- >      [, iirlen[, iskipinit]]]
--
-- doc: <http://www.csounds.com/manual/html/ftconv.html>
ftconv :: CsdTuple a => Sig -> Tab -> D -> a
ftconv = mopc3 "ftconv" (as 8, [a,i,i,i,i,i])

-- | A direct convolution opcode. 
--
-- > ares dconv asig, isize, ifn
--
-- doc: <http://www.csounds.com/manual/html/dconv.html>
dconv :: Sig -> D -> Tab -> Sig
dconv = opc3 "dconv" [(a, [a,i,i])]

-----------------------------------------------------
-- * FFT and Spectral Processing

-----------------------------------------------------
-- ** Realtime Analysis And Resynthesis

-- | Generate an fsig from a mono audio source ain, using phase vocoder overlap-add analysis. 
--
-- > fsig pvsanal ain, ifftsize, ioverlap, iwinsize, iwintype [, iformat] [, iinit]
--
-- doc: <http://www.csounds.com/manual/html/pvsanal.html>
pvsanal :: Sig -> D -> D -> D -> D -> Spec
pvsanal = opc5 "pvsanal" [(f, a:is 6)]

-- | pvstanal implements phase vocoder analysis by reading function tables containing sampled-sound sources, with GEN01, and pvstanal will accept deferred allocation tables.
-- 
-- This opcode allows for time and frequency-independent scaling. Time is advanced internally, but controlled by a tempo scaling parameter; when an onset is detected, 
-- timescaling is momentarily stopped to avoid smearing of attacks. The quality of the effect is generally improved with phase locking switched on.
-- 
-- pvstanal will also scale pitch, independently of frequency, using a transposition factor (k-rate). 
--
-- > fsig pvstanal ktimescal, kamp, kpitch, ktab, [kdetect, kwrap, ioffset,ifftsize, ihop, idbthresh]
--
-- doc: <http://www.csounds.com/manual/html/pvstanal.html>
pvstanal :: Sig -> Sig -> Sig -> Sig -> Spec
pvstanal = opc4 "pvstanal" [(f, ks 6 ++ is 4)]

-- | Resynthesise phase vocoder data (f-signal) using a FFT overlap-add. 
--
-- > ares pvsynth fsrc, [iinit]
--
-- doc: <http://www.csounds.com/manual/html/pvsynth.html>
pvsynth :: Spec -> Sig
pvsynth = opc1 "pvsynth" [(a, [f,i])]

-- | Resynthesize using a fast oscillator-bank. 
--
-- > ares pvsadsyn fsrc, inoscs, kfmod [, ibinoffset] [, ibinincr] [, iinit]
--
-- doc: <http://www.csounds.com/manual/html/pvsadsyn.html>
pvsadsyn :: Spec -> D -> Sig -> Sig
pvsadsyn = opc3 "pvsadsyn" [(a, [f,i,k,i,i,i])]

-----------------------------------------------------
-- ** Writing FFT Data To A File And Reading From It

-- | This opcode writes a fsig to a PVOCEX file (which in turn can be read by pvsfread or other programs that support PVOCEX file input). 
--
-- > pvsfwrite fsig, ifile
--
-- doc: <http://www.csounds.com/manual/html/pvsfwrite.html>
pvswrite :: Spec -> Str -> SE ()
pvswrite a1 a2 = se_ $ opc2 "pvswrite" [(x, [f,s])] a1 a2

-- | Create an fsig stream by reading a selected channel from a PVOC-EX analysis file loaded into memory, with frame 
-- interpolation. Only format 0 files (amplitude+frequency) are currently supported. The operation of this opcode 
-- mirrors that of pvoc, but outputs an fsig instead of a resynthesized signal. 
--
-- > fsig pvsfread ktimpt, ifn [, ichan]
--
-- doc: <http://www.csounds.com/manual/html/pvsfread.html>
pvsfread :: Sig -> Str -> Spec
pvsfread = opc2 "pvsfread" [(f, [k,s,i])]

-- | Create an fsig stream by reading a selected channel from a PVOC-EX analysis file, with frame interpolation. 
--
-- > fsig pvsdiskin SFname,ktscal,kgain[,ioffset, ichan]
--
-- doc: <http://www.csounds.com/manual/html/pvsdiskin.html>
pvsdiskin :: Str -> Sig -> Sig -> Spec
pvsdiskin = opc3 "pvsdiskin" [(f, [s,k,k,i,i])]

-----------------------------------------------------
-- ** FFT Info

-- | Get format information about fsrc, whether created by an opcode such as pvsanal, or obtained from 
-- a PVOCEX file by pvsfread. This information is available at init time, and can be used to set parameters 
-- for other pvs opcodes, and in particular for creating function tables (e.g. for pvsftw), or setting 
-- the number of oscillators for pvsadsyn. 
--
-- > ioverlap, inumbins, iwinsize, iformat pvsinfo fsrc
--
-- doc: <http://www.csounds.com/manual/html/pvsinfo.html>
pvsinfo :: Spec -> (D, D, D, D)
pvsinfo = mopc1 "pvsinfo" ([i,i,i,i], [f])

-- | Obtain the amp and freq values off a PVS signal bin as k-rate variables. 
--
-- > kamp, kfr pvsbin fsig, kbin
--
-- doc: <http://www.csounds.com/manual/html/pvsbin.html>
pvsbin :: Spec -> Sig -> (Sig, Sig)
pvsbin = mopc2 "pvsbin" ([k,k], [f,k])

-- | Calculate the spectral centroid of a signal from its discrete Fourier transform. 
--
-- > kcent pvscent fsig
--
-- doc: <http://www.csounds.com/manual/html/pvscent.html>
pvscent :: Spec -> Sig
pvscent = opc1 "pvscent" [(k, [f])]

-----------------------------------------------------
-- ** Manipulating FFT Signals

-- | Scale the frequency components of a pv stream, resulting in pitch shift. Output amplitudes can be optionally modified in order to attempt formant preservation. 
--
-- > fsig pvscale fsigin, kscal[, kkeepform, kgain, kcoefs]
--
-- doc: <http://www.csounds.com/manual/html/pvscale.html>
pvscale :: Spec -> Sig -> Spec
pvscale = opc2 "pvscale" [(f, [f,k,k,k,k])]

-- | Shift the frequency components of a pv stream, stretching/compressing its spectrum. 
--
-- > fsig pvshift fsigin, kshift, klowest[, kkeepform, igain, kcoefs]
--
-- doc: <http://www.csounds.com/manual/html/pvshift.html>
pvshift :: Spec -> Sig -> Sig -> Spec
pvshift = opc3 "pvshift" [(f, [f,k,k,k,i,k])]

-- | Filter the pvoc frames, passing bins whose frequency is within a band, and with linear interpolation for transitional bands. 
--
-- > fsig pvsbandp fsigin, xlowcut, xlowfull, \
-- >       xhighfull, xhighcut[, ktype]
--
-- doc: <http://www.csounds.com/manual/html/pvsbandp.html>
pvsbandp :: Spec -> Sig -> Sig -> Sig -> Sig -> Spec 
pvsbandp = opc5 "pvsbandp" [(f, [f,x,x,x,x,k])]

-- | Filter the pvoc frames, rejecting bins whose frequency is within a band, and with linear interpolation for transitional bands. 
--
-- > fsig pvsbandr fsigin, xlowcut, xlowfull, \
-- >       xhighfull, xhighcut[, ktype]
--
-- doc: <http://www.csounds.com/manual/html/pvsbandr.html>
pvsbandr :: Spec -> Sig -> Sig -> Sig -> Sig -> Spec 
pvsbandr = opc5 "pvsbandr" [(f, [f,x,x,x,x,k])]

-- | Mix 'seamlessly' two pv signals. This opcode combines the most prominent components of two pvoc streams into a single mixed stream. 
--
-- > fsig pvsmix fsigin1, fsigin2
--
-- doc: <http://www.csounds.com/manual/html/pvsmix.html>
pvsmix :: Spec -> Spec -> Spec
pvsmix = opc2 "pvsmix" [(f, [f,f])]

-- | Performs cross-synthesis between two source fsigs. 
--
-- > fsig pvscross fsrc, fdest, kamp1, kamp2
--
-- doc: <http://www.csounds.com/manual/html/pvscross.html>
pvscross :: Spec -> Spec -> Sig -> Sig -> Spec
pvscross = opc4 "pvscross" [(f, [f,f,k,k])]

-- | Multiply amplitudes of a pvoc stream by those of a second pvoc stream, with dynamic scaling. 
--
-- > fsig pvsfilter fsigin, fsigfil, kdepth[, igain]
--
-- doc: <http://www.csounds.com/manual/html/pvsfilter.html>
pvsfilter :: Spec -> Spec -> Sig -> Spec
pvsfilter = opc3 "pvsfilter" [(f, [f,f,k,i])]

-- | This opcode provides support for cross-synthesis of amplitudes and frequencies. It takes the amplitudes 
-- of one input fsig and combines with frequencies from another. It is a spectral version of the well-known channel vocoder. 
--
-- > fsig pvsvoc famp, fexc, kdepth, kgain [,kcoefs]
--
-- doc: <http://www.csounds.com/manual/html/pvsvoc.html>
pvsvoc :: Spec -> Spec -> Sig -> Sig -> Spec
pvsvoc = opc4 "pvsvoc" [(f, [f,f,k,k,k])]

-- | Performs morphing (or interpolation) between two source fsigs. 
--
-- > fsig pvsmorph fsrc, fdest, kamp1, kamp2
--
-- doc: <http://www.csounds.com/manual/html/pvsmotph.html>
pvsmorph :: Spec -> Spec -> Sig -> Sig -> Spec
pvsmorph = opc4 "pvsmorph" [(f, [f,f,k,k])]

-- | This opcodes 'freezes' the evolution of pvs stream by locking into steady amplitude and/or frequency 
-- values for each bin. The freezing is controlled, independently for amplitudes and frequencies, by a 
-- control-rate trigger, which switches the freezing 'on' if equal to or above 1 and 'off' if below 1. 
--
-- > fsig pvsfreeze fsigin, kfreeza, kfreezf
--
-- doc: <http://www.csounds.com/manual/html/pvsfreeze.html>
pvsfreeze :: Spec -> Sig -> Sig -> Sig
pvsfreeze = opc3 "pvsfreeze" [(f, [f,k,k])]

-- | Modify amplitudes of fsrc using function table, with dynamic scaling. 
--
-- > fsig pvsmaska fsrc, ifn, kdepth
--
-- doc: <http://www.csounds.com/manual/html/pvsmaska.html>
pvsmaska :: Spec -> Tab -> Sig -> Spec
pvsmaska = opc3 "pvsmaska" [(f, [f,i,k])]

-- | Average the amp/freq time functions of each analysis channel for a specified time (truncated to number of frames). As a side-effect the input pvoc stream will be delayed by that amount. 
--
-- > fsig pvsblur fsigin, kblurtime, imaxdel
--
-- doc: <http://www.csounds.com/manual/html/pvsblur.html>
pvsblur :: Spec -> Sig -> D -> Spec
pvsblur = opc3 "pvsblur" [(f, [f,k,i])]

-- | Transforms a pvoc stream according to a masking function table; if the pvoc stream amplitude 
-- falls below the value of the function for a specific pvoc channel, it applies a gain to that channel.
-- 
-- The pvoc stream amplitudes are compared to a masking table, if the fall below the table values, they
-- are scaled by kgain. Prior to the operation, table values are scaled by klevel, which can be used as masking depth control.
-- 
-- Tables have to be at least fftsize/2 in size; for most GENS it is important to use an extended-guard 
-- point (size power-of-two plus one), however this is not necessary with GEN43.
-- 
-- One of the typical uses of pvstencil would be in noise reduction. A noise print can be analysed with 
-- pvanal into a PVOC-EX file and loaded in a table with GEN43. This then can be used as the masking table
-- for pvstencil and the amount of reduction would be controlled by kgain. Skipping post-normalisation will 
-- keep the original noise print average amplitudes. This would provide a good starting point for a successful 
-- noise reduction (so that klevel can be generally set to close to 1).
-- 
-- Other possible transformation effects are possible, such as filtering and `inverse-masking'.
--
-- > fsig pvstencil fsigin, kgain, klevel, iftable
--
-- doc: <http://www.csounds.com/manual/html/pvstencil.html>
pvstencil :: Spec -> Sig -> Sig -> Tab -> Sig
pvstencil = opc4 "pvstencil" [(f, [f,k,k,i])]

-- | This opcode arpeggiates spectral components, by amplifying one bin and attenuating all the others around 
-- it. Used with an LFO it will provide a spectral arpeggiator similar to Trevor Wishart's CDP program specarp. 
--
-- > fsig pvsarp fsigin, kbin, kdepth, kgain
--
-- doc: <http://www.csounds.com/manual/html/pvsarp.html>
pvsarp :: Spec -> Sig -> Sig -> Sig -> Spec
pvsarp = opc4 "pvsarp" [(f, [f,k,k,k])]

-- | Smooth the amplitude and frequency time functions of a pv stream using a 1st order lowpass IIR with time-varying 
-- cutoff frequency. This opcode uses the same filter as the tone opcode, but this time acting separately on the amplitude
-- and frequency time functions that make up a pv stream. The cutoff frequency parameter runs at the control-rate, but unlike 
-- tone and tonek, it is not specified in Hz, but as fractions of 1/2 frame-rate (actually the pv stream sampling rate),
-- which is easier to understand. This means that the highest cutoff frequency is 1 and the lowest 0; the lower the 
-- frequency the smoother the functions and more pronounced the effect will be.
-- 
-- These are filters applied to control signals so the effect is basically blurring the spectral evolution. The effects 
-- produced are more or less similar to pvsblur, but with two important differences: 1.smoothing of amplitudes and 
-- frequencies use separate sets of filters; and 2. there is no increase in computational cost when higher amounts 
-- of "blurring" (smoothing) are desired. 
--
-- > fsig pvsmooth fsigin, kacf, kfcf
--
-- doc: <http://www.csounds.com/manual/html/pvsmooth.html>
pvsmooth :: Spec -> Sig -> Sig -> Spec
pvsmooth = opc3 "pvsmooth" [(f, [f,k,k])]

-----------------------------------------------------
-- * Physical Models and FM Instruments

-----------------------------------------------------
-- ** Waveguide Physical Modelling

-- | An audio signal is modified by a string resonator with variable fundamental frequency. 
--
-- > ares streson asig, kfr, ifdbgain
--
-- doc: <http://www.csounds.com/manual/html/streson.html>
streson :: Sig -> Sig -> D -> Sig
streson = opc3 "streson" [(a, [a,k,i])]

-- | Audio output is a naturally decaying plucked string or drum sound based on the Karplus-Strong algorithms. 
--
-- > ares pluck kamp, kcps, icps, ifn, imeth [, iparm1] [, iparm2]     
--
-- doc: <http://www.csounds.com/manual/html/pluck.html>
pluck :: Amp -> Cps -> Icps -> Tab -> D -> Sig
pluck = opc5 "pluck" [(a, [k,k,i,i,i,i,i])]

-- | repluck is an implementation of the physical model of the plucked string. A user can control the pluck point, 
-- the pickup point, the filter, and an additional audio signal, axcite. axcite is used to excite the 'string'. Based on the Karplus-Strong algorithm. 
--
-- > ares repluck iplk, kamp, icps, kpick, krefl, axcite
--
-- doc: <http://www.csounds.com/manual/html/repluck.html>
repluck :: D -> Amp -> Icps -> Sig -> Sig -> Sig -> Sig
repluck = opc6 "repluck" [(a, [i,k,i,k,k,a])]

-- | Audio output is a tone similar to a bowed string, using a physical model developed from Perry Cook, but re-coded for Csound. 
--
-- > ares wgbow kamp, kfreq, kpres, krat, kvibf, kvamp, ifn [, iminfreq]
--
-- doc: <http://www.csounds.com/manual/html/wgbow.html>
wgbow :: Amp -> Cps -> Sig -> Sig -> Sig -> Sig -> Tab -> Sig
wgbow = opc7 "wgbow" [(a, ks 6 ++ is 2)]

-- | A physical model of a bowed bar, belonging to the Perry Cook family of waveguide instruments. 
--
-- > ares wgbowedbar kamp, kfreq, kpos, kbowpres, kgain [, iconst] [, itvel] \
-- >      [, ibowpos] [, ilow]
--
-- doc: <http://www.csounds.com/manual/html/wgbowedbar.html>
wgbowedbar :: Amp -> Cps -> Sig -> Sig -> Sig -> Sig
wgbowedbar = opc5 "wgbowedbar" [(a, ks 5 ++ is 2)]

-- | Audio output is a tone related to a brass instrument, using a physical model developed from Perry Cook, but re-coded for Csound. 
--
-- > ares wgbrass kamp, kfreq, ktens, iatt, kvibf, kvamp, ifn [, iminfreq]   
--
-- doc: <http://www.csounds.com/manual/html/wgbrass.html>
wgbrass :: Amp -> Cps -> Sig -> D -> Sig -> Sig -> Tab -> Sig 
wgbrass = opc7 "wgbrass" [(a, [k,k,k,i,k,k,i,i])]

-- | Audio output is a tone similar to a clarinet, using a physical model developed from Perry Cook, but re-coded for Csound. 
--
-- > ares wgclar kamp, kfreq, kstiff, iatt, idetk, kngain, kvibf, kvamp, ifn \
-- >       [, iminfreq]
--
-- doc: <http://www.csounds.com/manual/html/wgclar.html>
wgclar :: Amp -> Cps -> Sig -> D -> D -> Sig -> Sig -> Sig -> Tab -> Sig
wgclar = opc9 "wgclar" [(a, [k,k,k,i,i,k,k,k,i,i])]

-- | Audio output is a tone similar to a flute, using a physical model developed from Perry Cook, but re-coded for Csound. 
--
-- > ares wgflute kamp, kfreq, kjet, iatt, idetk, kngain, kvibf, kvamp, ifn \
-- >      [, iminfreq] [, ijetrf] [, iendrf]
--
-- doc: <http://www.csounds.com/manual/html/wgflute.html>
wgflute :: Amp -> Cps -> Sig -> D -> D -> Sig -> Sig -> Sig -> Tab -> Sig
wgflute = opc9 "wgflute" [(a, [k,k,k,i,i,k,k,k,i,i,i,i])]

-- | A high fidelity simulation of a plucked string, using interpolating delay-lines. 
--
-- > ares wgpluck icps, iamp, kpick, iplk, idamp, ifilt, axcite
--
-- doc: <http://www.csounds.com/manual/html/wgpluck.html>
wgpluck :: Icps -> Iamp -> Sig -> D -> D -> D -> Sig -> Sig
wgpluck = opc7 "wgplusk" [(a, [i,i,k,i,i,i,a])]

-- | wgpluck2 is an implementation of the physical model of the plucked string, with control over the pluck point, the pickup point and the filter. Based on the Karplus-Strong algorithm. 
--
-- > ares wgpluck2 iplk, kamp, icps, kpick, krefl
--
-- doc: <http://www.csounds.com/manual/html/wgpluck2.html>
wgpluck2 :: D -> Amp -> D -> Sig -> Sig -> Sig
wgpluck2 = opc5 "wgpluck2" [(a, [i,k,i,k,k])]

-- | A simple waveguide model consisting of one delay-line and one first-order lowpass filter. 
--
-- > ares wguide1 asig, xfreq, kcutoff, kfeedback
--
-- doc: <http://www.csounds.com/manual/html/wguide1.html>
wguide1 :: Amp -> Cps -> Sig -> Sig -> Sig
wguide1 = opc4 "wguide1" [(a, [a,x,k,k])]

-- | A model of beaten plate consisting of two parallel delay-lines and two first-order lowpass filters. 
--
-- > ares wguide2 asig, xfreq1, xfreq2, kcutoff1, kcutoff2, \
-- >       kfeedback1, kfeedback2
--
-- doc: <http://www.csounds.com/manual/html/wguide2.html>
wguide2 ::  Amp -> Cps -> Cps -> Sig -> Sig -> Sig -> Sig -> Sig
wguide2 = opc7 "wguide2" [(a, [a,x,x,k,k,k,k])]
  
----------------------------------------------------
-- ** FM Instrument Models


fmGen :: Name -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Tab -> Tab -> Tab -> Sig
fmGen name = opc11 name [(a, ks 6 ++ is 6)]


-- | Uses FM synthesis to create a Hammond B3 organ sound. It comes from a family of FM sounds, all using 4 basic oscillators and various architectures, as used in the TX81Z synthesizer. 
--
-- > ares fmb3 kamp, kfreq, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, \
-- >      ifn4, ivfn
--
-- doc: <http://www.csounds.com/manual/html/fmb3.html>
fmb3 :: Amp -> Cps -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Tab -> Tab -> Tab -> Sig


-- | Uses FM synthesis to create a tublar bell sound. It comes from a family of FM sounds, all using 4 basic oscillators and various architectures, as used in the TX81Z synthesizer. 
--
-- > ares fmbell kamp, kfreq, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, \
-- >       ifn4, ivfn[, isus]
--
-- doc: <http://www.csounds.com/manual/html/fmbell.html>
fmbell :: Amp -> Cps -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Tab -> Tab -> Tab -> Sig

-- | Uses FM synthesis to create a “Heavy Metal” sound. It comes from a family of FM sounds, all using 4 basic oscillators and various architectures, as used in the TX81Z synthesizer. 
--
-- > ares fmmetal kamp, kfreq, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, \
-- >       ifn4, ivfn
--
-- doc: <http://www.csounds.com/manual/html/fmmetal.html>
fmmetal :: Amp -> Cps -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Tab -> Tab -> Tab -> Sig

-- | Uses FM synthesis to create a percussive flute sound. It comes from a family of FM sounds, all using 4 basic oscillators and various architectures, as used in the TX81Z synthesizer. 
--
-- > ares fmpercfl kamp, kfreq, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, \
-- >       ifn3, ifn4, ivfn
--
-- doc: <http://www.csounds.com/manual/html/fmpercfl.html>
fmpercfl :: Amp -> Cps -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Tab -> Tab -> Tab -> Sig

-- | Uses FM synthesis to create a Fender Rhodes electric piano sound. It comes from a family of FM sounds, all using 4 basic oscillators and various architectures, as used in the TX81Z synthesizer. 
--
-- > ares fmrhode kamp, kfreq, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, \
-- >       ifn3, ifn4, ivfn
--
-- doc: <http://www.csounds.com/manual/html/fmrhode.html>

fmrhode :: Amp -> Cps -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Tab -> Tab -> Tab -> Sig

-- | FM Singing Voice Synthesis 
--
-- > ares fmvoice kamp, kfreq, kvowel, ktilt, kvibamt, kvibrate, ifn1, \
-- >      ifn2, ifn3, ifn4, ivibfn
--
-- doc: <http://www.csounds.com/manual/html/fmvoice.html>
fmvoice :: Amp -> Cps -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Tab -> Tab -> Tab -> Sig


fmwurlie :: Amp -> Cps -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Tab -> Tab -> Tab -> Sig
      
fmb3 = fmGen "fmb3"
fmbell = fmGen "fmbell"
fmmetal = fmGen "fmmetal"
fmpercfl = fmGen "fmpercfl"
fmrhode = fmGen "fmrhode"
fmvoice = fmGen "fmvoice"
fmwurlie = fmGen "fmwurlie"

--------------------------------------------------------------------
-- PhISEM percussion opcodes. 

-- | bamboo is a semi-physical model of a bamboo sound.
-- 
-- > ares bamboo kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] \
-- >      [, ifreq1] [, ifreq2]
--
-- doc: <http://www.csounds.com/manual/html/bamboo.html> 

bamboo :: Amp -> D -> Sig
bamboo = opc2 "bamboo" [(a, k:is 7)]

-- | cabasa is a semi-physical model of a cabasa sound.
--
-- > ares cabasa iamp, idettack [, inum] [, idamp] [, imaxshake]
--
-- doc: <http://www.csounds.com/manual/html/cabasa.html> 
cabasa :: Iamp -> D -> Sig
cabasa = opc2 "cabasa" [(a, is 5)]

-- | crunch is a semi-physical model of a crunch sound.
--
-- > ares crunch iamp, idettack [, inum] [, idamp] [, imaxshake]
--
-- doc: <http://www.csounds.com/manual/html/crunch.html> 

crunch :: Iamp -> D -> Sig
crunch = opc2 "crunch" [(a, is 5)]

-- | dripwater is a semi-physical model of a water drop.
--
-- > ares dripwater kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] \
-- >       [, ifreq1] [, ifreq2]
--
-- doc: <http://www.csounds.com/manual/html/dripwater.html> 

dripwater :: Amp -> D -> Sig
dripwater = opc2 "dripwater" [(a, k:is 7)]

-- | guiro is a semi-physical model of a guiro sound.
--
-- > ares guiro kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] [, ifreq1]
--
-- doc: <http://www.csounds.com/manual/html/guiro.html> 

guiro :: Amp -> D -> Sig
guiro = opc2 "guiro" [(a, k:is 7)]

-- | sandpaper is a semi-physical model of a sandpaper sound.
--
-- > ares sandpaper iamp, idettack [, inum] [, idamp] [, imaxshake]
--
-- doc: <http://www.csounds.com/manual/html/sandpaper.html> 

sandpaper :: Iamp -> D -> Sig
sandpaper = opc2 "sandpaper" [(a, is 5)]

-- | sekere is a semi-physical model of a sekere sound.
--
-- > ares sekere iamp, idettack [, inum] [, idamp] [, imaxshake]
--
-- doc: <http://www.csounds.com/manual/html/sekere.html> 

sekere :: Iamp -> D -> Sig
sekere = opc2 "sekere" [(a, is 5)]

-- | sleighbells is a semi-physical model of a sleighbell sound.
--
-- > ares sleighbells kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] \
-- >      [, ifreq1] [, ifreq2]
--
-- doc: <http://www.csounds.com/manual/html/sleighbells.html> 

sleighbells :: Amp -> D -> Sig
sleighbells = opc2 "sleighbells" [(a, k:is 7)]

-- | stix is a semi-physical model of a stick sound.
--
-- > ares stix iamp, idettack [, inum] [, idamp] [, imaxshake]
--
-- doc: <http://www.csounds.com/manual/html/stix.html> 

stix :: Iamp -> D -> Sig
stix = opc2 "stix" [(a, is 5)]

-- | tambourine is a semi-physical model of a tambourine sound.
--
-- > ares tambourine kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] \
-- >      [, ifreq1] [, ifreq2]
--
-- doc: <http://www.csounds.com/manual/html/tambourine.html> 
tambourine :: Amp -> D -> Sig
tambourine = opc2 "tambourine" [(a, k:is 7)]

-------------------------------------------------------------
-- by Perry Cook

-- | Audio output is a tone related to the striking of a cow bell or similar. 
--
-- > ares gogobel kamp, kfreq, ihrd, ipos, imp, kvibf, kvamp, ivfn
--
-- doc: <http://www.csounds.com/manual/html/gogobel.html> 

gogobel :: Amp -> Cps -> D -> D -> D -> Sig -> Sig -> Tab -> Sig
gogobel = opc8 "gogobel" [(a, [k,k,i,i,i,k,k,i])]


-- | Audio output is a tone related to the striking of a wooden block as found in a marimba.
--
-- > ares marimba kamp, kfreq, ihrd, ipos, imp, kvibf, kvamp, ivibfn, idec \
-- >      [, idoubles] [, itriples]
--
-- doc: <http://www.csounds.com/manual/html/marimba.html> 

marimba :: Amp -> Cps -> D -> D -> Tab -> Sig -> Sig -> Tab -> D -> Sig
marimba = opc9 "marimba" [(a, [k,k,i,i,i,k,k,i,i,i,i])]

-- | Audio output is a tone related to the shaking of a maraca or similar gourd instrument.
--
-- > ares shaker kamp, kfreq, kbeans, kdamp, ktimes [, idecay]
--
-- doc: <http://www.csounds.com/manual/html/shaker.html> 

shaker :: Amp -> Cps -> Sig -> Sig -> Sig -> Sig
shaker = opc5 "shaker" [(a, ks 5 ++ [i])]


-- | Audio output is a tone related to the striking of a metal block as found in a vibraphone.
--
-- > ares vibes kamp, kfreq, ihrd, ipos, imp, kvibf, kvamp, ivibfn, idec
--
-- doc: <http://www.csounds.com/manual/html/vibes.html> 

vibes :: Amp -> Cps -> D -> D -> Tab -> Sig -> Sig -> Tab -> D -> Sig
vibes = opc9 "vibes" [(a, [k,k,i,i,i,k,k,i,i])]

---------------------------------------------------------------
-- other models


-- | Audio output is a tone similar to a struck metal bar, using a physical model developed from solving the partial
-- differential equation. There are controls over the boundary conditions as well as the bar characteristics. 
--
-- > ares barmodel kbcL, kbcR, iK, ib, kscan, iT30, ipos, ivel, iwid
--
-- doc: <http://www.csounds.com/manual/html/barmodel.html> 
barmodel :: Sig -> Sig -> D -> D -> Sig -> D -> D -> D -> D -> Sig
barmodel = opc9 "barmodel" [(a, [k,k,i,i,k,i,i,i,i])]

-- | An emulation of a mandolin. 
--
-- > ares mandol kamp, kfreq, kpluck, kdetune, kgain, ksize, ifn [, iminfreq]
--
-- doc: <http://www.csounds.com/manual/html/mandol.html> 

mandol :: Amp -> Cps -> Sig -> Sig -> Sig -> Sig -> Tab -> Sig
mandol = opc7 "mandol" [(a, ks 6 ++ is 2)]


-- | An emulation of a mini-Moog synthesizer. 
--
-- > ares moog kamp, kfreq, kfiltq, kfiltrate, kvibf, kvamp, iafn, iwfn, ivfn
--
-- doc: <http://www.csounds.com/manual/html/moog.html> 

moog :: Amp -> Cps -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Tab -> Sig
moog = opc9 "moog" [(a, ks 6 ++ is 3)]
 

-- | An emulation of a human voice. 
--
-- > ares voice kamp, kfreq, kphoneme, kform, kvibf, kvamp, ifn, ivfn
--
-- doc: <http://www.csounds.com/manual/html/voice.html> 

voice :: Amp -> Cps -> Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Sig
voice = opc9 "voice" [(a, [k,k,k,k,k,k,k,i,i])]
      

