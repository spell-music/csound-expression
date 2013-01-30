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
    flanger, harmon, phaser1, phaser2, pdclip, pdhalf, pdhalfy 

    -- ** Doppler Shift

    -----------------------------------------------------
    -- * Granular Synthesis

    -----------------------------------------------------
    -- * Convolution

    -----------------------------------------------------
    -- * FFT and Spectral Processing

    -- ** Realtime Analysis And Resynthesis

    -- ** Writing FFT Data To A File And Reading From It

    -- ** FFT Info

    -- ** Manipulating FFT Signals
   
    -----------------------------------------------------
    -- * Physical Models and FM Instruments

    -- ** Waveguide Physical Modelling

    -- ** FM Instrument Models
) where


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
crossfmGen name = mopc7 name [a, a] [x, x, x, x, k, i, i, i, i]

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

-----------------------------------------------------
-- * FFT and Spectral Processing

-----------------------------------------------------
-- ** Realtime Analysis And Resynthesis

-----------------------------------------------------
-- ** Writing FFT Data To A File And Reading From It

-----------------------------------------------------
-- ** FFT Info

-----------------------------------------------------
-- ** Manipulating FFT Signals

-----------------------------------------------------
-- * Physical Models and FM Instruments

-----------------------------------------------------
-- ** Waveguide Physical Modelling

-----------------------------------------------------
-- ** FM Instrument Models

