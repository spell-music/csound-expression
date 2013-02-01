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
    dopler,
    
    -----------------------------------------------------
    -- * Granular Synthesis

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
    fmb3, fmbell, fmmetal, fmpercfl, fmrhode, fmvoice, fmwurlie 
    
    -- ** Other Models
) where

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.Cons

i = Ir
k = Kr
a = Ar
x = Xr
s = Sr
f = Fr
is n = replicate n i
ks n = replicate n k
as n = replicate n a 


-----------------------------------------------------
-- * Modulation and Distortion

-----------------------------------------------------
-- ** Frequency Modulation

-- ares foscil xamp, kcps, xcar, xmod, kndx, ifn [, iphs]
foscil :: Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Sig
foscil = opc6 "foscil" [(a, [x, k, x, x, k, i, i])]

-- ares foscili xamp, kcps, xcar, xmod, kndx, ifn [, iphs]
foscili :: Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Sig
foscili = opc6 "foscili" [(a, [x, k, x, x, k, i, i])]

-- a1, a2 crossfm xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
-- a1, a2 crossfmi xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
-- a1, a2 crosspm xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
-- a1, a2 crosspmi xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
-- a1, a2 crossfmpm xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
-- a1, a2 crossfmpmi xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]

crossfmGen :: Name -> Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> (Sig, Sig)
crossfmGen name = mopc7 name ([a, a], [x, x, x, x, k, i, i, i, i])

crossfm, crossfmi, crosspm, crosspmi, crossfmpm, crossfmpmi :: Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> (Sig, Sig)

crossfm = crossfmGen "crossfm"
crossfmi = crossfmGen "crossfmi"
crosspm = crossfmGen "crosspm"
crosspmi = crossfmGen "crosspmi"
crossfmpm = crossfmGen "crossfmpm"
crossfmpmi = crossfmGen "crossfmpmi"

-----------------------------------------------------
-- ** Distortion and Wave Shaping

-- ar distort asig, kdist, ifn[, ihp, istor]
distort :: Sig -> Sig -> Tab -> Sig
distort = opc3 "distort" [(a, [a, k, i, i, i])]

-- ares distort1 asig, kpregain, kpostgain, kshape1, kshape2[, imode]
distort1 :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
distort1 = opc5 "distort1" [(a, [a, k, k, k, k, i])]

-- aout powershape ain, kShapeAmount [, ifullscale]
powershape :: Sig -> Sig -> Sig
powershape = opc2 "powershape" [(a, [a, k, i])]

-- aout polynomial ain, k0 [, k1 [, k2 [...]]]
polynomial :: Sig -> [Sig] -> Sig
polynomial a1 a2 = opcs "polynomial" [(a, a:repeat k)] (a1:a2)

-- aout chebyshevpoly ain, k0 [, k1 [, k2 [...]]]
chebyshevpoly :: Sig -> [Sig] -> Sig
chebyshevpoly a1 a2 = opcs "chebyshevpoly" [(a, a:repeat k)] (a1:a2)

-----------------------------------------------------
-- ** Flanging, Phasing, Phase Shaping

-- ares flanger asig, adel, kfeedback [, imaxd]
flanger :: Sig -> Sig -> Sig -> Sig
flanger = opc3 "flanger" [(a, [a, a, k, i])]

-- ares harmon asig, kestfrq, kmaxvar, kgenfreq1, kgenfreq2, imode, \
--       iminfrq, iprd
harmon :: Sig -> Sig -> Sig -> Sig -> Sig -> D -> D -> D -> Sig  
harmon = opc8 "harmon" [(a, [a, k, k, k, k, i, i, i])]

-- ares phaser1 asig, kfreq, kord, kfeedback [, iskip]
phaser1 :: Sig -> Sig -> Sig -> Sig -> Sig
phaser1 = opc4 "phaser1" [(a, [a, k, k, k, i])]

-- ares phaser2 asig, kfreq, kq, kord, kmode, ksep, kfeedback
phaser2 :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
phaser2 = opc7 "phaser2" [(a, [a, k, k, k, k, k, k])]

-- aout pdclip ain, kWidth, kCenter [, ibipolar [, ifullscale]]
pdclip :: Sig -> Sig -> Sig -> Sig
pdclip = opc3 "pdclip" [(a, [a, k, k, i, i])]

-- aout pdhalf ain, kShapeAmount [, ibipolar [, ifullscale]]
pdhalf :: Sig -> Sig -> Sig -> Sig
pdhalf = opc3 "pdhalf" [(a, [a, k, k, i, i])]

-- aout pdhalfy ain, kShapeAmount [, ibipolar [, ifullscale]]
pdhalfy :: Sig -> Sig -> Sig -> Sig
pdhalfy = opc3 "pdhalfy" [(a, [a, k, k, i, i])]

-----------------------------------------------------
-- ** Doppler Shift

-- ashifted doppler asource, ksourceposition, kmicposition [, isoundspeed, ifiltercutoff]
dopler :: Sig -> Sig -> Sig -> Sig
dopler = opc3 "dopler" [(a, [a, k, k, i, i])]

-----------------------------------------------------
-- * Granular Synthesis

-----------------------------------------------------
-- * Convolution

-- ar1 [, ar2] [, ar3] [, ar4] pconvolve ain, ifilcod [, ipartitionsize, ichannel]
pconvolve :: MultiOut a => Sig -> S -> a
pconvolve = mopc2 "pconvolve" ([a,a,a,a], [a,s,i,i])

-- ar1 [, ar2] [, ar3] [, ar4] convolve ain, ifilcod [, ichannel]
convolve :: MultiOut a => Sig -> D -> a
convolve = mopc2 "convolve" ([a, a, a, a], [a, i, i])

-- a1[, a2[, a3[, ... a8]]] ftconv ain, ift, iplen[, iskipsamples \
--      [, iirlen[, iskipinit]]]
ftconv :: MultiOut a => Sig -> Tab -> D -> a
ftconv = mopc3 "ftconv" (as 8, [a,i,i,i,i,i])

-- ares dconv asig, isize, ifn
dconv :: Sig -> I -> Tab -> Sig
dconv = opc3 "dconv" [(a, [a,i,i])]

-----------------------------------------------------
-- * FFT and Spectral Processing

-----------------------------------------------------
-- ** Realtime Analysis And Resynthesis

-- fsig pvsanal ain, ifftsize, ioverlap, iwinsize, iwintype [, iformat] [, iinit]
pvsanal :: Sig -> I -> I -> I -> I -> Spec
pvsanal = opc5 "pvsanal" [(f, a:is 6)]

-- fsig pvstanal ktimescal, kamp, kpitch, ktab, [kdetect, kwrap, ioffset,ifftsize, ihop, idbthresh]
pvstanal :: Sig -> Sig -> Sig -> Sig -> Spec
pvstanal = opc4 "pvstanal" [(f, ks 6 ++ is 4)]

-- ares pvsynth fsrc, [iinit]
pvsynth :: Spec -> Sig
pvsynth = opc1 "pvsynth" [(a, [f,i])]

-- ares pvsadsyn fsrc, inoscs, kfmod [, ibinoffset] [, ibinincr] [, iinit]
pvsadsyn :: Spec -> I -> Sig -> Sig
pvsadsyn = opc3 "pvsadsyn" [(a, [f,i,k,i,i,i])]

-----------------------------------------------------
-- ** Writing FFT Data To A File And Reading From It

-- pvsfwrite fsig, ifile
pvswrite :: Spec -> S -> SE ()
pvswrite a1 a2 = se_ $ opc2 "pvswrite" [(x, [f,s])] a1 a2

-- fsig pvsfread ktimpt, ifn [, ichan]
pvsfread :: Sig -> S -> Spec
pvsfread = opc2 "pvsfread" [(f, [k,s,i])]

-- fsig pvsdiskin SFname,ktscal,kgain[,ioffset, ichan]
pvsdiskin :: S -> Sig -> Sig -> Spec
pvsdiskin = opc3 "pvsdiskin" [(f, [s,k,k,i,i])]

-----------------------------------------------------
-- ** FFT Info

-- ioverlap, inumbins, iwinsize, iformat pvsinfo fsrc
pvsinfo :: Spec -> (I, I, I, I)
pvsinfo = mopc1 "pvsinfo" ([i,i,i,i], [f])

-- kamp, kfr pvsbin fsig, kbin
pvsbin :: Spec -> Sig -> (Sig, Sig)
pvsbin = mopc2 "pvsbin" ([k,k], [f,k])

-- kcent pvscent fsig
pvscent :: Spec -> Sig
pvscent = opc1 "pvscent" [(k, [f])]

-----------------------------------------------------
-- ** Manipulating FFT Signals

-- fsig pvscale fsigin, kscal[, kkeepform, kgain, kcoefs]
pvscale :: Spec -> Sig -> Spec
pvscale = opc2 "pvscale" [(f, [f,k,k,k,k])]

-- fsig pvshift fsigin, kshift, klowest[, kkeepform, igain, kcoefs]
pvshift :: Spec -> Sig -> Sig -> Spec
pvshift = opc3 "pvshift" [(f, [f,k,k,k,i,k])]

-- fsig pvsbandp fsigin, xlowcut, xlowfull, \
--       xhighfull, xhighcut[, ktype]
pvsbandp :: Spec -> Sig -> Sig -> Sig -> Sig -> Spec 
pvsbandp = opc5 "pvsbandp" [(f, [f,x,x,x,x,k])]

-- fsig pvsbandr fsigin, xlowcut, xlowfull, \
--       xhighfull, xhighcut[, ktype]
pvsbandr :: Spec -> Sig -> Sig -> Sig -> Sig -> Spec 
pvsbandr = opc5 "pvsbandr" [(f, [f,x,x,x,x,k])]

-- fsig pvsmix fsigin1, fsigin2
pvsmix :: Spec -> Spec -> Spec
pvsmix = opc2 "pvsmix" [(f, [f,f])]

-- fsig pvscross fsrc, fdest, kamp1, kamp2
pvscross :: Spec -> Spec -> Sig -> Sig -> Spec
pvscross = opc4 "pvscross" [(f, [f,f,k,k])]

-- fsig pvsfilter fsigin, fsigfil, kdepth[, igain]
pvsfilter :: Spec -> Spec -> Sig -> Spec
pvsfilter = opc3 "pvsfilter" [(f, [f,f,k,i])]

-- fsig pvsvoc famp, fexc, kdepth, kgain [,kcoefs]
pvsvoc :: Spec -> Spec -> Sig -> Sig -> Spec
pvsvoc = opc4 "pvsvoc" [(f, [f,f,k,k,k])]

-- fsig pvsmorph fsrc, fdest, kamp1, kamp2
pvsmorph :: Spec -> Spec -> Sig -> Sig -> Spec
pvsmorph = opc4 "pvsmorph" [(f, [f,f,k,k])]

-- fsig pvsfreeze fsigin, kfreeza, kfreezf
pvsfreeze :: Spec -> Sig -> Sig -> Sig
pvsfreeze = opc3 "pvsfreeze" [(f, [f,k,k])]

-- fsig pvsmaska fsrc, ifn, kdepth
pvsmaska :: Spec -> Tab -> Sig -> Spec
pvsmaska = opc3 "pvsmaska" [(f, [f,i,k])]

-- fsig pvsblur fsigin, kblurtime, imaxdel
pvsblur :: Spec -> Sig -> D -> Spec
pvsblur = opc3 "pvsblur" [(f, [f,k,i])]

-- fsig pvstencil fsigin, kgain, klevel, iftable
pvstencil :: Spec -> Sig -> Sig -> Tab -> Sig
pvstencil = opc4 "pvstencil" [(f, [f,k,k,i])]

-- fsig pvsarp fsigin, kbin, kdepth, kgain
pvsarp :: Spec -> Sig -> Sig -> Sig -> Spec
pvsarp = opc4 "pvsarp" [(f, [f,k,k,k])]

-- fsig pvsmooth fsigin, kacf, kfcf
pvsmooth :: Spec -> Sig -> Sig -> Spec
pvsmooth = opc3 "pvsmooth" [(f, [f,k,k])]

-----------------------------------------------------
-- * Physical Models and FM Instruments

-----------------------------------------------------
-- ** Waveguide Physical Modelling

-- ares streson asig, kfr, ifdbgain
streson :: Sig -> Sig -> D -> Sig
streson = opc3 "streson" [(a, [a,k,i])]

-- ares pluck kamp, kcps, icps, ifn, imeth [, iparm1] [, iparm2]     
pluck :: Sig -> Sig -> D -> Tab -> I -> Sig
pluck = opc5 "pluck" [(a, [k,k,i,i,i,i,i])]

-- ares repluck iplk, kamp, icps, kpick, krefl, axcite
repluck :: D -> Sig -> D -> Sig -> Sig -> Sig -> Sig
repluck = opc6 "repluck" [(a, [i,k,i,k,k,a])]

-- ares wgbow kamp, kfreq, kpres, krat, kvibf, kvamp, ifn [, iminfreq]
wgbow :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Sig
wgbow = opc7 "wgbow" [(a, ks 6 ++ is 2)]

-- ares wgbowedbar kamp, kfreq, kpos, kbowpres, kgain [, iconst] [, itvel] \
--      [, ibowpos] [, ilow]
wgbowedbar :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
wgbowedbar = opc5 "wgbowedbar" [(a, ks 5 ++ is 2)]

-- ares wgbrass kamp, kfreq, ktens, iatt, kvibf, kvamp, ifn [, iminfreq]   
wgbrass :: Sig -> Sig -> Sig -> D -> Sig -> Sig -> Tab -> Sig 
wgbrass = opc7 "wgbrass" [(a, [k,k,k,i,k,k,i,i])]

-- ares wgclar kamp, kfreq, kstiff, iatt, idetk, kngain, kvibf, kvamp, ifn \
--       [, iminfreq]
wgclar :: Sig -> Sig -> Sig -> D -> D -> Sig -> Sig -> Sig -> Tab -> Sig
wgclar = opc9 "wgclar" [(a, [k,k,k,i,i,k,k,k,i,i])]

-- ares wgflute kamp, kfreq, kjet, iatt, idetk, kngain, kvibf, kvamp, ifn \
--      [, iminfreq] [, ijetrf] [, iendrf]
wgflute :: Sig -> Sig -> Sig -> D -> D -> Sig -> Sig -> Sig -> Tab -> Sig
wgflute = opc9 "wgflute" [(a, [k,k,k,i,i,k,k,k,i,i,i,i])]

-- ares wgpluck icps, iamp, kpick, iplk, idamp, ifilt, axcite
wgpluck :: D -> D -> Sig -> D -> D -> D -> Sig -> Sig
wgpluck = opc7 "wgplusk" [(a, [i,i,k,i,i,i,a])]

-- ares wgpluck2 iplk, kamp, icps, kpick, krefl
wgpluck2 :: D -> Sig -> D -> Sig -> Sig -> Sig
wgpluck2 = opc5 "wgpluck2" [(a, [i,k,i,k,k])]

-- ares wguide1 asig, xfreq, kcutoff, kfeedback
wguide1 :: Sig -> Sig -> Sig -> Sig -> Sig
wguide1 = opc4 "wguide1" [(a, [a,x,k,k])]

-- ares wguide2 asig, xfreq1, xfreq2, kcutoff1, kcutoff2, \
--       kfeedback1, kfeedback2
wguide2 ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
wguide2 = opc7 "wguide2" [(a, [a,x,x,k,k,k,k])]
  
----------------------------------------------------
-- ** FM Instrument Models

-- ares fmvoice kamp, kfreq, kvowel, ktilt, kvibamt, kvibrate, ifn1, \
--      ifn2, ifn3, ifn4, ivibfn

fmGen :: Name -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Tab -> Tab -> Tab -> Sig
fmGen name = opc11 name [(a, ks 6 ++ is 6)]

fmb3, fmbell, fmmetal, fmpercfl, fmrhode, 
    fmvoice, fmwurlie :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Tab -> Tab -> Tab -> Sig
      
fmb3 = fmGen "fmb3"
fmbell = fmGen "fmbell"
fmmetal = fmGen "fmmetal"
fmpercfl = fmGen "fmpercfl"
fmrhode = fmGen "fmrhode"
fmvoice = fmGen "fmvoice"
fmwurlie = fmGen "fmwurlie"
      

