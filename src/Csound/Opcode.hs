module Csound.Opcode where

import qualified Data.Map as M

import Csound.Exp.Cons
import Csound.Render.Sco(Msg)
import Csound.Exp.Wrapper hiding (Double', Int', String')
import Csound.Exp

-- handy shortcuts

i = Ir
k = Kr
a = Ar
x = Xr
s = Sr

clip :: Sig -> I -> D -> Sig
clip = opc3 "clip" [(a, [a, i, i, i])]   
                  
zeroDbfs :: D
zeroDbfs = opc0 "0dbfs" [(i, [])]

--------------------------------------------
-- handful of opcodes


-- oscillators

oscil :: Sig -> Sig -> Tab -> Sig
oscil = opc3 "oscil" [
    (a, [x, x, i, i]),
    (k, [k, k, i, i])]

poscil :: Sig -> Sig -> Tab -> Sig
poscil = opc3 "poscil" [
    (a, [x, x, i, i]),
    (k, [k, k, i, i])]

-- ares vco2 kamp, kcps [, imode] [, kpw] [, kphs] [, inyx]
vco2 :: Sig -> Sig -> Sig
vco2 = opc2 "vco2" [(a, [k, k, i, k, k, i])]

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

---------------------------------------------------
-- panning

-- a1, a2 pan2 asig, xp [, imode]
pan2 :: Sig -> Sig -> (Sig, Sig)
pan2 = mopc2 "pan2" ([a, a], [a, x, i])

---------------------------------------------------
-- reverberation

-- aoutL, aoutR freeverb ainL, ainR, kRoomSize, kHFDamp[, iSRate[, iSkip]] 
freeverb :: Sig -> Sig -> Sig -> Sig -> (Sig, Sig)
freeverb = mopc4 "freeverb" ([a, a], [a, a, k, k, i, i])

-- aoutL, aoutR reverbsc ainL, ainR, kfblvl, kfco[, israte[, ipitchm[, iskip]]] 
reverbsc :: Sig -> Sig -> Sig -> Sig -> (Sig, Sig)
reverbsc = mopc4 "reverbsc" ([a, a], [a, a, k, k, i, i, i])

---------------------------------------------------
-- waveshaping

-- ares table andx, ifn [, ixmode] [, ixoff] [, iwrap]
-- ires table indx, ifn [, ixmode] [, ixoff] [, iwrap]
-- kres table kndx, ifn [, ixmode] [, ixoff] [, iwrap]
table, tablei :: Sig -> Tab -> Sig

table = mkTable "table"
tablei = mkTable "tablei"

mkTable :: Name -> Sig -> Tab -> Sig
mkTable name = opc2 name [
    (a, a:rest),
    (k, k:rest),
    (i, i:rest)]
    where rest = [i, i, i]

-- ar distort asig, kdist, ifn[, ihp, istor]
distort :: Sig -> Sig -> Tab -> Sig
distort = opc3 "distort" [(a, [a, k, i, i, i])]

---------------------------------------------------
-- convolution

-- ar1 [, ar2] [, ar3] [, ar4] pconvolve ain, ifilcod [, ipartitionsize, ichannel]
pconvolve :: MultiOut a => Sig -> D -> a
pconvolve = mopc2 "pconvolve" ([a, a, a, a], [a, i, i, i])

-- ar1 [, ar2] [, ar3] [, ar4] convolve ain, ifilcod [, ichannel]
convolve :: MultiOut a => Sig -> D -> a
convolve = mopc2 "convolve" ([a, a, a, a], [a, i, i])

-- a1[, a2[, a3[, ... a8]]] ftconv ain, ift, iplen[, iskipsamples \
--      [, iirlen[, iskipinit]]]
ftconv :: MultiOut a => Sig -> D -> D -> a
ftconv = mopc3 "ftconv" (replicate 8 a, a:replicate 5 i)

---------------------------------------------------
-- filters

filterSignature1 = [(a, [a, k, i])]
mkFilter1 name = opc2 name filterSignature1

filterSignature2 = [(a, [a, k, k, i])]
mkFilter2 name = opc3 name filterSignature2

tone, atone, butlp, buthp :: Sig -> Sig -> Sig
reson, butbp, butbr, moogladder :: Sig -> Sig -> Sig -> Sig

tone  = mkFilter1 "tone"
atone = mkFilter1 "atone"
butlp = mkFilter1 "butlp"
buthp = mkFilter1 "buthp"

reson = mkFilter2 "reson"
butbp = mkFilter2 "butbp"
butbr = mkFilter2 "butbr"
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

----------------------------------------------------
-- 

xtratim :: D -> SE ()
xtratim a1 = se_ $ opc1 "xtratim" [(x, [i])] a1

----------------------------------------------------
-- delay lines

delayr :: D -> SE Sig
delayr a1 = se $ opc1 "delayr" [(a, [i])] a1

delayw :: Sig -> SE ()
delayw a1 = se_ $ opc1 "delayw" [(x, [a])] a1

deltap :: Sig -> SE Sig
deltap a1 = se $ opc1 "deltap" [(a, [k])] a1

----------------------------------------------------
-- random opcodes

-- ares rand xamp [, iseed] [, isel] [, ioffset]
-- kres rand xamp [, iseed] [, isel] [, ioffset]
rand :: Sig -> SE Sig
rand sig = se $ opc1 "rand" [
    (a, x:rest),
    (k, k:rest)] sig
    where rest = replicate 3 i


-- ares noise xamp, kbeta
noise :: Sig -> Sig -> SE Sig
noise a1 a2 = se $ opc2 "noise" [(a, [x, k])] a1 a2

-- ares pinkish xin [, imethod] [, inumbands] [, iseed] [, iskip]
pinkish :: Sig -> Sig 
pinkish = opc1 "pinkish" [(a, x: replicate 4 i)]

----------------------------------------------------
-- read files

soundin :: MultiOut a => S -> a
soundin = mopc1 "soundin" (repeat a, [s])

soundin2 :: S -> (Sig, Sig) 
soundin2 = soundin

diskin :: MultiOut a => S -> Sig -> a
diskin = mopc2 "diskin2" (repeat a, s:k:replicate 6 i)

diskin1 :: S -> Sig -> Sig
diskin1 = diskin

diskin2 :: S -> Sig -> (Sig, Sig)
diskin2 = diskin

diskin4 :: S -> Sig -> (Sig, Sig, Sig, Sig)
diskin4 = diskin

idur :: D
idur = p 3

gen :: Int -> Int -> [Double] -> Tab
gen = Ftable 

----------------------------------------------------
-- midi 

cpsmidi :: Msg -> D
cpsmidi = const $ constOpc "cpsmidi"

ampmidi :: Msg -> D
ampmidi = const $ constOpc "ampmidi"

pchbend :: Msg -> Sig 
pchbend = const $ constDiap "pchbend"

aftouch :: Msg -> Sig
aftouch = const $ constDiap "aftouch"

ctrl7 :: I -> I -> Sig -> Sig -> Sig
ctrl7 = opc4 "ctrl7" [
    (i, replicate 5 i),
    (k, [i, i, k, k, i]),
    (a, [i, i, k, k, i, i])]


constOpc :: Val a => Name -> a
constOpc name = opc0 name [(i, [])]

constDiap :: Val a => Name -> a
constDiap name = opc0 name [(k, [i, i])]

-------------------------------------------------
-- io

fout :: [Sig] -> SE ()
fout as = se_ $ opcs "fout" [(x, repeat a)] as
 




