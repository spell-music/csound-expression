-- | Basic signal processing
module Csound.Opcode.Basic(
    -----------------------------------------------------
    -- * Oscillators and phasors

    -- ** Standard Oscillators
    oscils, poscil, poscil3, oscil, oscili, oscil3,

    -- ** Dynamic Sprectrum Oscillators
    buzz, gbuzz, mpulse, vco, vco2,  

    -- ** Phasors
    phasor, syncphasor,

    -----------------------------------------------------
    -- * Random and Noise generators    
    rand, randi, randh, rnd31, random, randomi, randomh, pinkish, noise,

    -----------------------------------------------------
    -- * Envelopes
    linseg, expseg, linsegr, expsegr,
    lpshold, loopseg, looptseg,    

    -----------------------------------------------------
    -- * Delays

    -- ** Audio delays
    vdelay, vdelayx, vdelayxw,
    delayr, delayw, deltap, deltapi, deltap3, deltapx, deltapxw,

    -- ** Control delays

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
    moogladder, vcomb, bqrez, comb,

    -----------------------------------------------------
    -- * Reverb
    freeverb, reverbsc, reverb, nreverb, babo, 

    -----------------------------------------------------
    -- * Signal Measurement, Dynamic Processing, Sample Level Operations

    -- ** Amplitude Measurement And Following
    rms, balance, follow, follow2, peak, max_k,

    -- ** Pitch Estimation
    ptrack, pitch, pitchamdf, pvscent,  

    -- ** Tempo Estimation

    -- ** Dynamic Processing
    compress, dam, clip, 

    -- ** Sample Level Operations
    limit, samphold, vaget, vaset,  

    -----------------------------------------------------
    -- * Spatialization

    -- ** Panning
    pan, pan2,

    -- ** VBAP

    -- ** Ambisonics

    -- ** Binaural / HTRF
    hrtfstat, hrtfmove, hrtfmove2

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

-----------------------------------------------------
-- Standard Oscillators

-- ares oscils iamp, icps, iphs [, iflg]

oscils :: D -> D -> D -> Sig
oscils = opc3 "oscils" [(a, [i, i, i])]

oscGen :: Name -> Sig -> Sig -> Tab -> Sig 
oscGen name = opc3 name [
    (a, [x, x, i, i]),
    (k, [k, k, i, i])]


oscil :: Sig -> Sig -> Tab -> Sig
oscil = oscGen "oscil"

oscili :: Sig -> Sig -> Tab -> Sig
oscili = oscGen "oscili"

oscil3 :: Sig -> Sig -> Tab -> Sig
oscil3 = oscGen "oscil3"

poscil :: Sig -> Sig -> Tab -> Sig
poscil = oscGen "psocil"

poscil3 :: Sig -> Sig -> Tab -> Sig
poscil3 = oscGen "psocil"

-----------------------------------------------------
-- Dynamic Sprectrum Oscillators

-- buzz, gbuzz, mpulse, vco, vco2  

-- ares buzz xamp, xcps, knh, ifn [, iphs]
buzz :: Sig -> Sig -> Sig -> Tab -> Sig
buzz = opc4 "buzz" [(a, [x, x, k, i, i])]

-- ares gbuzz xamp, xcps, knh, klh, kmul, ifn [, iphs]
gbuzz :: Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Sig
gbuzz = opc6 "gbuzz" [(a, [x, x, k, k, k, i, i])]

-- ares mpulse kamp, kintvl [, ioffset]
mpulse :: Sig -> Sig -> Sig
mpulse = opc2 "mpulse" [(a, [k, k, i])]

-- ares vco xamp, xcps, iwave, kpw [, ifn] [, imaxd] [, ileak] [, inyx] \
--      [, iphs] [, iskip]
vco :: Sig -> Sig -> I -> Sig -> Sig
vco = opc4 "vco" [(a, [x, x, i, k] ++ is 6)]

-- ares vco2 kamp, kcps [, imode] [, kpw] [, kphs] [, inyx]
vco2 :: Sig -> Sig -> Sig
vco2 = opc2 "vco2" [(a, [k, k, i, k, k, i])]


-----------------------------------------------------
-- Phasors

-- ares phasor xcps [, iphs]
-- kres phasor kcps [, iphs]
phasor :: Sig -> Sig 
phasor = opc1 "phasor" [
    (a, [x, i]),
    (k, [x, i])]

-- aphase, asyncout syncphasor xcps, asyncin, [, iphs]
syncphasor :: Sig -> Sig -> (Sig, Sig)
syncphasor = mopc2 "syncphasor" ([a, a], [x, a, i])


-----------------------------------------------------
-- Random

-- ares rand xamp [, iseed] [, isel] [, ioffset]
-- kres rand xamp [, iseed] [, isel] [, ioffset]
rand :: Sig -> SE Sig
rand sig = se $ opc1 "rand" [
    (a, x:rest),
    (k, k:rest)] sig
    where rest = is 3

-- ares randi xamp, xcps [, iseed] [, isize] [, ioffset]
-- kres randi kamp, kcps [, iseed] [, isize] [, ioffset]

randi :: Sig -> Sig -> SE Sig
randi = randiGen "randi"

randh :: Sig -> Sig -> SE Sig
randh = randiGen "randh"

randiGen :: Name -> Sig -> Sig -> SE Sig
randiGen name a1 a2 = se $ opc2 name [
    (a, x:x:rest),
    (k, k:k:rest)] a1 a2
    where rest = is 3

-- ax rnd31 kscl, krpow [, iseed]
-- ix rnd31 iscl, irpow [, iseed]
-- kx rnd31 kscl, krpow [, iseed]
rnd31 :: Sig -> Sig -> SE Sig
rnd31 a1 a2 = se $ opc2 "rnd31" [
    (a, [k, k, i]),
    (k, [k, k, i]),
    (i, [i, i, i])] a1 a2

-- ax random kscl, krpow
-- ix random iscl, irpow
-- kx random kscl, krpow
random :: Sig -> Sig -> SE Sig
random a1 a2 = se $ opc2 "random" [
    (a, [k, k]),
    (k, [k, k]),
    (i, [i, i])] a1 a2

-- ares randomi kmin, kmax, xcps [,imode] [,ifirstval]
-- kres randomi kmin, kmax, kcps [,imode] [,ifirstval]
randomi :: Sig -> Sig -> Sig -> SE Sig
randomi a1 a2 a3 = se $ opc3 "randomi" [
    (a, [k, k, x, i, i]),
    (k, [k, k, k, i, i])] a1 a2 a3

-- ares randomh kmin, kmax, xcps [,imode] [,ifirstval]
-- kres randomh kmin, kmax, kcps [,imode] [,ifirstval]
randomh :: Sig -> Sig -> Sig -> SE Sig
randomh a1 a2 a3 = se $ opc3 "randomh" [
    (a, [k, k, x, i, i]),
    (k, [k, k, k, i, i])] a1 a2 a3

-- ares pinkish xin [, imethod] [, inumbands] [, iseed] [, iskip]
pinkish :: Sig -> SE Sig 
pinkish a1 = se $ opc1 "pinkish" [(a, x: replicate 4 i)] a1

-- ares noise xamp, kbeta
noise :: Sig -> Sig -> SE Sig
noise a1 a2 = se $ opc2 "noise" [(a, [x, k])] a1 a2

--------------------------------------------------
-- envelopes

linseg :: [D] -> Sig
linseg = opcs "linseg" [
    (a, repeat i),
    (k, repeat i)]

linsegr :: [D] -> D -> D -> Sig
linsegr xs relDur relVal = opcs "linsegr" ([
    (a, repeat i),
    (k, repeat i)]) (xs ++ [relDur, relVal])

expseg :: [D] -> Sig
expseg = opcs "expseg" [
    (a, repeat i),
    (k, repeat i)]

expsegr :: [D] -> D -> D -> Sig
expsegr xs relDur relVal = opcs "expsegr" ([
    (a, repeat i),
    (k, repeat i)]) (xs ++ [relDur, relVal])


-- ksig lpshold kfreq, ktrig, ktime0, kvalue0  [, ktime1] [, kvalue1] \
--       [, ktime2] [, kvalue2] [...]
lpshold, loopseg, looptseg :: Sig -> Sig -> [Sig] -> Sig

lpshold = mkLps "lpshold"
loopseg = mkLps "loopseg"
looptseg = mkLps "looptseg"

mkLps :: Name -> Sig -> Sig -> [Sig] -> Sig
mkLps name kfreq ktrig kvals = opcs name signature $ kfreq:ktrig:kvals
    where signature = [(k, repeat k)]


----------------------------------------------------
-- audio delays

-- ares vdelay asig, adel, imaxdel [, iskip]
vdelay :: Sig -> Sig -> D -> Sig
vdelay = opc3 "vdelay" [(a, [a, a, i, i])]

-- aout vdelayx ain, adl, imd, iws [, ist]
vdelayx :: Sig -> Sig -> D -> D -> Sig
vdelayx = opc4 "vdelayx" [(a, [a, a, i, i, i])]

-- aout vdelayxw ain, adl, imd, iws [, ist]
vdelayxw :: Sig -> Sig -> D -> D -> Sig
vdelayxw = opc4 "vdelayxw" [(a, [a, a, i, i, i])]

----------------------------------------------------
-- delay lines

delayr :: D -> SE Sig
delayr a1 = se $ opc1 "delayr" [(a, [i])] a1

delayw :: Sig -> SE ()
delayw a1 = se_ $ opc1 "delayw" [(x, [a])] a1

deltap :: Sig -> SE Sig
deltap a1 = se $ opc1 "deltap" [(a, [k])] a1

deltapi :: Sig -> SE Sig
deltapi a1 = se $ opc1 "deltapi" [(a, [x])] a1

deltap3 :: Sig -> SE Sig
deltap3 a1 = se $ opc1 "deltap3" [(a, [x])] a1

-- aout deltapx adel, iwsize
deltapx :: Sig -> D -> SE Sig
deltapx a1 a2 = se $ opc2 "deltapx" [(a, [a, i])] a1 a2

-- deltapxw ain, adel, iwsize
deltapxw :: Sig -> Sig -> D -> SE ()
deltapxw a1 a2 a3 = se_ $ opc3 "deltapxw" [(x, [a, a, i])] a1 a2 a3

---------------------------------------------------
-- filters

filterSignature1 = [(a, [a, k, i])]
mkFilter1 name = opc2 name filterSignature1

filterSignature2 = [(a, [a, k, k, i])]
mkFilter2 name = opc3 name filterSignature2

tone, atone, butlp, buthp :: Sig -> Sig -> Sig
reson, areson, butbp, butbr, moogladder :: Sig -> Sig -> Sig -> Sig

tone  = mkFilter1 "tone"
atone = mkFilter1 "atone"
butlp = mkFilter1 "butlp"
buthp = mkFilter1 "buthp"

reson  = mkFilter2 "reson"
areson = mkFilter2 "areson"
butbp  = mkFilter2 "butbp"
butbr  = mkFilter2 "butbr"
moogladder = mkFilter2 "moogladder"

-- ares vcomb asig, krvt, xlpt, imaxlpt [, iskip] [, insmps]
vcomb :: Sig -> Sig -> Sig -> D -> Sig
vcomb = opc4 "vcomb" [(a, [a, k, x, i, i, i])]

-- ares bqrez asig, xfco, xres [, imode] [, iskip]
bqrez :: Sig -> Sig -> Sig -> Sig
bqrez = opc3 "bqrez" [(a, [a, x, x, i, i])]

-- ares comb asig, krvt, ilpt [, iskip] [, insmps]
comb :: Sig -> Sig -> D -> Sig
comb = opc3 "comb" [(a, [a, k, i, i, i])]

-- kres port ksig, ihtim [, isig]
port :: Sig -> D -> Sig
port = opc2 "port" [(k, [k, i, i])]

-- kres portk ksig, khtim [, isig]
portk :: Sig -> Sig -> Sig
portk = opc2 "portk" [(k, [k, k, i])]

---------------------------------------------------
-- reverberation

-- aoutL, aoutR freeverb ainL, ainR, kRoomSize, kHFDamp[, iSRate[, iSkip]] 
freeverb :: Sig -> Sig -> Sig -> Sig -> (Sig, Sig)
freeverb = mopc4 "freeverb" ([a, a], [a, a, k, k, i, i])

-- aoutL, aoutR reverbsc ainL, ainR, kfblvl, kfco[, israte[, ipitchm[, iskip]]] 
reverbsc :: Sig -> Sig -> Sig -> Sig -> (Sig, Sig)
reverbsc = mopc4 "reverbsc" ([a, a], [a, a, k, k, i, i, i])

-- ares reverb asig, krvt [, iskip]
reverb :: Sig -> Sig -> Sig
reverb = opc2 "reverb" [(a, [a, k, i])]

-- ares nreverb asig, ktime, khdif [, iskip] [,inumCombs] [, ifnCombs] \
--       [, inumAlpas] [, ifnAlpas]
nreverb :: Sig -> Sig -> Sig -> Sig
nreverb = opc3 "nreverb" [(a, [a, k, k] ++ is 5)]

-- a1, a2 babo asig, ksrcx, ksrcy, ksrcz, irx, iry, irz [, idiff] [, ifno]
babo :: Sig -> Sig -> Sig -> Sig -> D -> D -> D -> (Sig, Sig)
babo = mopc7 "babo" ([a, a], [a, k, k, k] ++ is 5)

---------------------------------------------------
-- Amplitude Measurement And Following

-- kres rms asig [, ihp] [, iskip]
rms :: Sig -> Sig
rms = opc1 "rms" [(k, [a, i, i])]

-- ares balance asig, acomp [, ihp] [, iskip]
balance :: Sig -> Sig -> Sig
balance = opc2 "balance" [(a, [a, a, i, i])]

-- ares follow asig, idt
follow :: Sig -> D -> Sig
follow = opc2 "follow" [(a, [a, i])]

-- ares follow2 asig, katt, krel
follow2 :: Sig -> Sig -> Sig -> Sig
follow2 = opc3 "follow2" [(a, [a, k, k])]

-- kres peak asig
-- kres peak ksig
peak :: Sig -> Sig
peak = opc1 "peak" [(k, [x])]

-- knumkout max_k asig, ktrig, itype
max_k :: Sig -> Sig -> I -> Sig
max_k = opc3 "max_k" [(k, [a, k, i])]

-------------------------------------------------
-- Pitch Estimation

-- kcps, kamp ptrack asig, ihopsize[,ipeaks]
ptrack :: Sig -> D -> (Sig, Sig)
ptrack = mopc2 "ptrack" ([k, k], [a, i, i])

-- koct, kamp pitch asig, iupdte, ilo, ihi, idbthresh [, ifrqs] [, iconf] \
--       [, istrt] [, iocts] [, iq] [, inptls] [, irolloff] [, iskip]
pitch :: Sig -> D -> D -> D -> D -> (Sig, Sig) 
pitch = mopc5 "pitch" ([k, k], a:is 12)

-- kcps, krms pitchamdf asig, imincps, imaxcps [, icps] [, imedi] \
--       [, idowns] [, iexcps] [, irmsmedi]
pitchamdf :: Sig -> D -> D -> (Sig, Sig)
pitchamdf = mopc3 "pitchamdf" ([k, k], a:is 8)

-- kcent pvscent fsig
pvscent :: Spec -> Sig
pvscent = opc1 "pvscent" [(k, [f])]

-- ktemp tempest kin, iprd, imindur, imemdur, ihp, ithresh, ihtim, ixfdbak, \
--       istartempo, ifn [, idisprd] [, itweek]
tempest :: Sig -> D -> D -> D -> D -> D -> D -> D -> D -> Tab -> Sig
tempest = opc10 "tempest" [(k, k:is 11)]

-------------------------------------------------
-- Dynamic Processing

-- ar compress aasig, acsig, kthresh, kloknee, khiknee, kratio, katt, krel, ilook
compress :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> Sig
compress = opc9 "compress" [(a, [a, a, k, k, k, k, k, k, i])]

-- ares dam asig, kthreshold, icomp1, icomp2, irtime, iftime
dam :: Sig -> Sig -> D -> D -> D -> D -> Sig
dam = opc6 "dam" [(a, a:k:is 4)]

-- ares clip asig, imeth, ilimit [, iarg]
clip :: Sig -> I -> D -> Sig
clip = opc3 "clip" [(a, [a, i, i])]

-------------------------------------------------
-- Sample Level Operations

-- ares limit asig, klow, khigh
-- ires limit isig, ilow, ihigh
-- kres limit ksig, klow, khigh
limit :: Sig -> Sig -> Sig -> Sig
limit = opc3 "limit" [
    (a, [a, k, k]),
    (k, [k, k, k]),
    (i, [i, i, i])]

-- ares samphold asig, agate [, ival] [, ivstor]
-- kres samphold ksig, kgate [, ival] [, ivstor]
samphold :: Sig -> Sig -> Sig
samphold = opc2 "samphold" [
    (a, [a, a, i, i]),
    (k, [k, k, i, i])]

-- kval vaget kndx, avar
vaget :: Sig -> Sig -> Sig
vaget = opc2 "vaget" [(k, [k, a])]

-- vaset kval, kndx, avar
vaset :: Sig -> Sig -> Sig -> SE ()
vaset a1 a2 a3 = se_ $ opc3 "vaset" [(x, [k, k, a])] a1 a2 a3

---------------------------------------------------
-- panning

-- a1, a2, a3, a4 pan asig, kx, ky, ifn [, imode] [, ioffset]
pan :: Sig -> Sig -> Sig -> Tab -> (Sig, Sig, Sig, Sig)
pan = mopc4 "pan" ([a, a, a, a], [a, k, k, i, i, i])

-- a1, a2 pan2 asig, xp [, imode]
pan2 :: Sig -> Sig -> (Sig, Sig)
pan2 = mopc2 "pan2" ([a, a], [a, x, i])


-- HRTF

-- aleft, aright hrtfstat asrc, iAz, iElev, ifilel, ifiler [,iradius, isr]
hrtfstat :: Sig -> D -> D -> S -> S -> (Sig, Sig)
hrtfstat = mopc5 "hrtfstat" ([a, a], a:s:i:i:s:is 2)

-- aleft, aright hrtfmove asrc, kAz, kElev, ifilel, ifiler [, imode, ifade, isr]
hrtfmove :: Sig -> Sig -> Sig -> S -> S -> (Sig, Sig)
hrtfmove = mopc5 "hrtfmove" ([a, a], a:k:k:s:s:is 3)

-- aleft, aright hrtfmove2 asrc, kAz, kElev, ifilel, ifiler [,ioverlap, iradius, isr]
hrtfmove2 :: Sig -> Sig -> Sig -> S -> S -> (Sig, Sig)
hrtfmove2 = mopc5 "hrtfmove2" ([a, a], a:k:k:s:s:is 3)


