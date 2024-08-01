module Csound.Typed.Opcode.SpectralProcessing (
  -- * STFT.
  ktableseg,
  pvadd,
  pvbufread,
  pvcross,
  pvinterp,
  pvoc,
  pvread,
  tableseg,
  tablexseg,
  vpvoc,

  -- * LPC.
  allpole,
  apoleparams,
  lpcanal,
  lpcfilter,
  lpfreson,
  lpinterp,
  lpread,
  lpreson,
  lpslot,
  pvslpc,
  resonbnk,

  -- * Non-Standard.
  specaddm,
  specdiff,
  specdisp,
  specfilt,
  spechist,
  specptrk,
  specscal,
  specsum,
  spectrum,

  -- * Streaming.
  binit,
  cudanal,
  cudasliding,
  cudasynth,
  part2txt,
  partials,
  pvs2array,
  pvsadsyn,
  pvsanal,
  pvsarp,
  pvsbandp,
  pvsbandr,
  pvsbandwidth,
  pvsbin,
  pvsblur,
  pvsbuffer,
  pvsbufread,
  pvsbufread2,
  pvscale,
  pvscent,
  pvsceps,
  pvscross,
  pvsdemix,
  pvsdiskin,
  pvsdisp,
  pvsfilter,
  pvsfread,
  pvsfreeze,
  pvsfromarray,
  pvsftr,
  pvsftw,
  pvsfwrite,
  pvsgain,
  pvshift,
  pvsifd,
  pvsin,
  pvsinfo,
  pvsinit,
  pvsmaska,
  pvsmix,
  pvsmooth,
  pvsmorph,
  pvsosc,
  pvsout,
  pvspitch,
  pvstanal,
  pvstencil,
  pvstrace,
  pvsvoc,
  pvswarp,
  pvsynth,
  resyn,
  sinsyn,
  tabifd,
  tradsyn,
  trcross,
  trfilter,
  trhighest,
  trlowest,
  trmix,
  trscale,
  trshift,
  trsplit,

  -- * ATS.
  atsAdd,
  atsAddnz,
  atsBufread,
  atsCross,
  atsInfo,
  atsInterpread,
  atsPartialtap,
  atsRead,
  atsReadnz,
  atsSinnoi,

  -- * Loris.
  lorismorph,
  lorisplay,
  lorisread,

  -- * Other.
  centroid,
  filescal,
  mincer,
  mp3scal,
  paulstretch,
  temposcal,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Csound.Dynamic
import Csound.Typed

-- STFT.

{- |
Deprecated.

Deprecated. Use the tableseg opcode instead.

>  ktableseg  ifn1, idur1, ifn2 [, idur2] [, ifn3] [...]

csound doc: <https://csound.com/docs/manual/ktableseg.html>
-}
ktableseg :: Tab -> D -> Tab -> SE ()
ktableseg b1 b2 b3 =
  SE $ join $ f <$> (lift . unTab) b1 <*> (lift . unD) b2 <*> (lift . unTab) b3
  where
    f a1 a2 a3 = opcsDep_ "ktableseg" [(Xr, (repeat Ir))] [a1, a2, a3]

{- |
Reads from a pvoc file and uses the data to perform additive synthesis.

pvadd reads from a pvoc file and uses the data to perform additive synthesis using an internal array of interpolating oscillators. The user supplies the wave table (usually one period of a sine wave), and can choose which analysis bins will be used in the re-synthesis.

> ares  pvadd  ktimpnt, kfmod, ifilcod, ifn, ibins [, ibinoffset] \
>           [, ibinincr] [, iextractmode] [, ifreqlim] [, igatefn]

csound doc: <https://csound.com/docs/manual/pvadd.html>
-}
pvadd :: Sig -> Sig -> Str -> Tab -> D -> Sig
pvadd b1 b2 b3 b4 b5 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unStr b3 <*> unTab b4 <*> unD b5
  where
    f a1 a2 a3 a4 a5 = opcs "pvadd" [(Ar, [Kr, Kr, Sr, Ir, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
Reads from a phase vocoder analysis file and makes the retrieved data available.

pvbufread reads from a pvoc file and makes the retrieved data available to any following pvinterp and pvcross units that appear in an instrument before a subsequent pvbufread (just as lpread and lpreson work together). The data is passed internally and the unit has no output of its own.

>  pvbufread  ktimpnt, ifile

csound doc: <https://csound.com/docs/manual/pvbufread.html>
-}
pvbufread :: Sig -> Str -> SE ()
pvbufread b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unStr) b2
  where
    f a1 a2 = opcsDep_ "pvbufread" [(Xr, [Kr, Sr])] [a1, a2]

{- |
Applies the amplitudes from one phase vocoder analysis file to the data from a second file.

pvcross applies the amplitudes from one phase vocoder analysis file to the data from a second file and then performs the resynthesis. The data is passed, as described above, from a previously called pvbufread unit. The two k-rate amplitude arguments are used to scale the amplitudes of each files separately before they are added together and used in the resynthesis (see below for further explanation). The frequencies of the first file are not used at all in this process. This unit simply allows for cross-synthesis through the application of the amplitudes of the spectra of one signal to the frequencies of a second signal. Unlike pvinterp, pvcross does allow for the use of the ispecwp as in pvoc and vpvoc.

> ares  pvcross  ktimpnt, kfmod, ifile, kampscale1, kampscale2 [, ispecwp]

csound doc: <https://csound.com/docs/manual/pvcross.html>
-}
pvcross :: Sig -> Sig -> Str -> Sig -> Sig -> Sig
pvcross b1 b2 b3 b4 b5 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unStr b3 <*> unSig b4 <*> unSig b5
  where
    f a1 a2 a3 a4 a5 = opcs "pvcross" [(Ar, [Kr, Kr, Sr, Kr, Kr, Ir])] [a1, a2, a3, a4, a5]

{- |
Interpolates between the amplitudes and frequencies of two phase vocoder analysis files.

pvinterp interpolates between the amplitudes and frequencies, on a bin by bin basis, of two phase vocoder analysis files (one from a previously called pvbufread unit and the other from within its own argument list), allowing for user defined transitions between analyzed sounds. It also allows for general scaling of the amplitudes and frequencies of each file separately before the interpolated values are calculated and sent to the resynthesis routines. The kfmod argument in pvinterp performs its frequency scaling on the frequency values after their derivation from the separate scaling and subsequent interpolation is performed so that this acts as an overall scaling value of the new frequency components.

> ares  pvinterp  ktimpnt, kfmod, ifile, kfreqscale1, kfreqscale2, \
>           kampscale1, kampscale2, kfreqinterp, kampinterp

csound doc: <https://csound.com/docs/manual/pvinterp.html>
-}
pvinterp :: Sig -> Sig -> Str -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
pvinterp b1 b2 b3 b4 b5 b6 b7 b8 b9 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unStr b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7 <*> unSig b8 <*> unSig b9
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
      opcs
        "pvinterp"
        [(Ar, [Kr, Kr, Sr, Kr, Kr, Kr, Kr, Kr, Kr])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        ]

{- |
Implements signal reconstruction using an fft-based phase vocoder.

> ares  pvoc  ktimpnt, kfmod, ifilcod [, ispecwp] [, iextractmode] \
>           [, ifreqlim] [, igatefn]

csound doc: <https://csound.com/docs/manual/pvoc.html>
-}
pvoc :: Sig -> Sig -> Str -> Sig
pvoc b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unStr b3
  where
    f a1 a2 a3 = opcs "pvoc" [(Ar, [Kr, Kr, Sr, Ir, Ir, Ir, Ir])] [a1, a2, a3]

{- |
Reads from a phase vocoder analysis file and returns the frequency and amplitude from a single analysis channel or bin.

pvread reads from a pvoc file and returns the frequency and amplitude from a single analysis channel or bin. The returned values can be used anywhere else in the Csound instrument. For example, one can use them as arguments to an oscillator to synthesize a single component from an analyzed signal or a bank of pvreads can be used to resynthesize the analyzed sound using additive synthesis by passing the frequency and magnitude values to a bank of oscillators.

> kfreq, kamp  pvread  ktimpnt, ifile, ibin

csound doc: <https://csound.com/docs/manual/pvread.html>
-}
pvread :: Sig -> Str -> D -> (Sig, Sig)
pvread b1 b2 b3 =
  pureTuple $ f <$> unSig b1 <*> unStr b2 <*> unD b3
  where
    f a1 a2 a3 = mopcs "pvread" ([Kr, Kr], [Kr, Sr, Ir]) [a1, a2, a3]

{- |
Creates a new function table by making linear segments between values in stored function tables.

tableseg is like linseg but interpolate between values in a stored function tables. The result is a new function table passed internally to any following vpvoc which occurs before a subsequent tableseg (much like lpread/lpreson pairs work). The uses of these are described below under vpvoc.

>  tableseg  ifn1, idur1, ifn2 [, idur2] [, ifn3] [...]

csound doc: <https://csound.com/docs/manual/tableseg.html>
-}
tableseg :: Tab -> D -> Tab -> SE ()
tableseg b1 b2 b3 =
  SE $ join $ f <$> (lift . unTab) b1 <*> (lift . unD) b2 <*> (lift . unTab) b3
  where
    f a1 a2 a3 = opcsDep_ "tableseg" [(Xr, (repeat Ir))] [a1, a2, a3]

{- |
Creates a new function table by making exponential segments between values in stored function tables.

tablexseg is like expseg but interpolate between values in a stored function tables. The result is a new function table passed internally to any following vpvoc which occurs before a subsequent tablexseg (much like lpread/lpreson pairs work). The uses of these are described below under vpvoc.

>  tablexseg  ifn1, idur1, ifn2 [, idur2] [, ifn3] [...]

csound doc: <https://csound.com/docs/manual/tablexseg.html>
-}
tablexseg :: Tab -> D -> Tab -> SE ()
tablexseg b1 b2 b3 =
  SE $ join $ f <$> (lift . unTab) b1 <*> (lift . unD) b2 <*> (lift . unTab) b3
  where
    f a1 a2 a3 = opcsDep_ "tablexseg" [(Xr, (repeat Ir))] [a1, a2, a3]

{- |
Implements signal reconstruction using an fft-based phase vocoder and an extra envelope.

> ares  vpvoc  ktimpnt, kfmod, ifile [, ispecwp] [, ifn]

csound doc: <https://csound.com/docs/manual/vpvoc.html>
-}
vpvoc :: Sig -> Sig -> Str -> Sig
vpvoc b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unStr b3
  where
    f a1 a2 a3 = opcs "vpvoc" [(Ar, [Kr, Kr, Sr, Ir, Ir])] [a1, a2, a3]

-- LPC.

--
-- > ares  allpole  asig, kCoef[]
--
-- csound doc: <https://csound.com/docs/manual/allpole.html>
allpole :: Sig -> Sig -> Sig
allpole b1 b2 =
  Sig $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = opcs "allpole" [(Ar, [Ar, Kr])] [a1, a2]

--
-- > kPar[] apoleparams  kCoef[]
--
-- csound doc: <https://csound.com/docs/manual/apoleparams.html>
apoleparams :: Sig -> Sig
apoleparams b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "apoleparams" [(Kr, [Kr])] [a1]

--
-- > kCoef[],krms,kerr,kcps  lpcanal  asrc, kflg,
-- >         kprd, isiz, iord[,iwin]
-- > kCoef[],krms,kerr,kcps  lpcanal  koff, kflg,
-- >         ifn, isiz, iord[,iwin]
-- > iCoef[],irms,ierr,icps  lpcanal  ioff, iflg,
-- >         ifn, isiz, iord[,iwin]
--
-- csound doc: <https://csound.com/docs/manual/lpcanal.html>
lpcanal :: forall a. (Tuple a) => Sig -> Sig -> Sig -> D -> D -> a
lpcanal b1 b2 b3 b4 b5 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4 <*> unD b5
  where
    f a1 a2 a3 a4 a5 = mopcs "lpcanal" ([Ir, Ir, Ir, Ir], [Ir, Ir, Ir, Ir, Ir, Ir]) [a1, a2, a3, a4, a5]

--
-- > ares  lpcfilter  asig, asrc, kflg,
-- >         kprd, isiz, iord[,iwin]
-- > ares  lpcfilter  asig, koff, kflg,
-- >         ifn, isiz, iord[,iwin]
--
-- csound doc: <https://csound.com/docs/manual/lpcfilter.html>
lpcfilter :: Sig -> Sig -> Sig -> Sig -> D -> D -> Sig
lpcfilter b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unD b5 <*> unD b6
  where
    f a1 a2 a3 a4 a5 a6 =
      opcs
        "lpcfilter"
        [ (Ar, [Ar, Ar, Kr, Kr, Ir, Ir, Ir])
        , (Ar, [Ar, Kr, Kr, Ir, Ir, Ir, Ir])
        ]
        [a1, a2, a3, a4, a5, a6]

{- |
Resynthesises a signal from the data passed internally by a previous lpread, applying formant shifting.

> ares  lpfreson  asig, kfrqratio

csound doc: <https://csound.com/docs/manual/lpfreson.html>
-}
lpfreson :: Sig -> Sig -> Sig
lpfreson b1 b2 =
  Sig $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = opcs "lpfreson" [(Ar, [Ar, Kr])] [a1, a2]

{- |
Computes a new set of poles from the interpolation between two analysis.

>  lpinterp  islot1, islot2, kmix

csound doc: <https://csound.com/docs/manual/lpinterp.html>
-}
lpinterp :: D -> D -> Sig -> SE ()
lpinterp b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep_ "lpinterp" [(Xr, [Ir, Ir, Kr])] [a1, a2, a3]

{- |
Reads a control file of time-ordered information frames.

> krmsr, krmso, kerr, kcps  lpread  ktimpnt, ifilcod [, inpoles] [, ifrmrate]

csound doc: <https://csound.com/docs/manual/lpread.html>
-}
lpread :: Sig -> Str -> (Sig, Sig, Sig, Sig)
lpread b1 b2 =
  pureTuple $ f <$> unSig b1 <*> unStr b2
  where
    f a1 a2 = mopcs "lpread" ([Kr, Kr, Kr, Kr], [Kr, Sr, Ir, Ir]) [a1, a2]

{- |
Resynthesises a signal from the data passed internally by a previous lpread.

> ares  lpreson  asig

csound doc: <https://csound.com/docs/manual/lpreson.html>
-}
lpreson :: Sig -> Sig
lpreson b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "lpreson" [(Ar, [Ar])] [a1]

{- |
Selects the slot to be use by further lp opcodes.

>  lpslot  islot

csound doc: <https://csound.com/docs/manual/lpslot.html>
-}
lpslot :: D -> SE ()
lpslot b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "lpslot" [(Xr, [Ir])] [a1]

--
-- > fsig  pvslpc  asrc, idftsiz, ihop, iord[,iwin]
--
-- csound doc: <https://csound.com/docs/manual/pvslpc.html>
pvslpc :: Sig -> D -> D -> D -> Spec
pvslpc b1 b2 b3 b4 =
  Spec $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "pvslpc" [(Fr, [Ar, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

--
-- > asig resonbnk  ain,
-- >         kPar[],kmin,kmax,iper[,imode,iscal,iskip]
--
-- csound doc: <https://csound.com/docs/manual/resonbnk.html>
resonbnk :: Sig -> Sig -> Sig
resonbnk b1 b2 =
  Sig $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = opcs "resonbnk" [(Ar, [Ar, Kr, Kr, Kr, Ir, Ir, Ir, Ir])] [a1, a2]

-- Non-Standard.

{- |
Perform a weighted add of two input spectra.

> wsig  specaddm  wsig1, wsig2 [, imul2]

csound doc: <https://csound.com/docs/manual/specaddm.html>
-}
specaddm :: Wspec -> Wspec -> Wspec
specaddm b1 b2 =
  Wspec $ f <$> unWspec b1 <*> unWspec b2
  where
    f a1 a2 = opcs "specaddm" [(Wr, [Wr, Wr, Ir])] [a1, a2]

{- |
Finds the positive difference values between consecutive spectral frames.

> wsig  specdiff  wsigin

csound doc: <https://csound.com/docs/manual/specdiff.html>
-}
specdiff :: Wspec -> Wspec
specdiff b1 =
  Wspec $ f <$> unWspec b1
  where
    f a1 = opcs "specdiff" [(Wr, [Wr])] [a1]

{- |
Displays the magnitude values of the spectrum.

>  specdisp  wsig, iprd [, iwtflg]

csound doc: <https://csound.com/docs/manual/specdisp.html>
-}
specdisp :: Wspec -> D -> SE ()
specdisp b1 b2 =
  SE $ join $ f <$> (lift . unWspec) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "specdisp" [(Xr, [Wr, Ir, Ir])] [a1, a2]

{- |
Filters each channel of an input spectrum.

> wsig  specfilt  wsigin, ifhtim

csound doc: <https://csound.com/docs/manual/specfilt.html>
-}
specfilt :: Wspec -> D -> Wspec
specfilt b1 b2 =
  Wspec $ f <$> unWspec b1 <*> unD b2
  where
    f a1 a2 = opcs "specfilt" [(Wr, [Wr, Ir])] [a1, a2]

{- |
Accumulates the values of successive spectral frames.

> wsig  spechist  wsigin

csound doc: <https://csound.com/docs/manual/spechist.html>
-}
spechist :: Wspec -> Wspec
spechist b1 =
  Wspec $ f <$> unWspec b1
  where
    f a1 = opcs "spechist" [(Wr, [Wr])] [a1]

{- |
Estimates the pitch of the most prominent complex tone in the spectrum.

Estimate the pitch of the most prominent complex tone in the spectrum.

> koct, kamp  specptrk  wsig, kvar, ilo, ihi, istr, idbthresh, inptls, \
>           irolloff [, iodd] [, iconfs] [, interp] [, ifprd] [, iwtflg]

csound doc: <https://csound.com/docs/manual/specptrk.html>
-}
specptrk :: Wspec -> Sig -> D -> D -> D -> D -> D -> D -> (Sig, Sig)
specptrk b1 b2 b3 b4 b5 b6 b7 b8 =
  pureTuple $ f <$> unWspec b1 <*> unSig b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unD b6 <*> unD b7 <*> unD b8
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 =
      mopcs
        "specptrk"
        ( [Kr, Kr]
        , [Wr, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir]
        )
        [a1, a2, a3, a4, a5, a6, a7, a8]

{- |
Scales an input spectral datablock with spectral envelopes.

> wsig  specscal  wsigin, ifscale, ifthresh

csound doc: <https://csound.com/docs/manual/specscal.html>
-}
specscal :: Wspec -> D -> D -> Wspec
specscal b1 b2 b3 =
  Wspec $ f <$> unWspec b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "specscal" [(Wr, [Wr, Ir, Ir])] [a1, a2, a3]

{- |
Sums the magnitudes across all channels of the spectrum.

> ksum  specsum  wsig [, interp]

csound doc: <https://csound.com/docs/manual/specsum.html>
-}
specsum :: Wspec -> Sig
specsum b1 =
  Sig $ f <$> unWspec b1
  where
    f a1 = opcs "specsum" [(Kr, [Wr, Ir])] [a1]

{- |
Generate a constant-Q, exponentially-spaced DFT.

Generate a constant-Q, exponentially-spaced DFT across all octaves of a multiply-downsampled control or audio input signal.

> wsig  spectrum  xsig, iprd, iocts, ifrqa [, iq] [, ihann] [, idbout] \
>           [, idsprd] [, idsinrs]

csound doc: <https://csound.com/docs/manual/spectrum.html>
-}
spectrum :: Sig -> D -> D -> D -> Wspec
spectrum b1 b2 b3 b4 =
  Wspec $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "spectrum" [(Wr, [Xr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

-- Streaming.

{- |
PVS tracks to amplitude+frequency conversion.

The binit opcode takes an input containg a TRACKS pv streaming signal (as generated,
  for instance by partials) and converts it into a equal-bandwidth bin-frame containing amplitude
  and frequency pairs (PVS_AMP_FREQ), suitable for overlap-add resynthesis (such as performed by
  pvsynth) or further PVS streaming phase vocoder signal transformations. For each frequency bin,
  it will look for a suitable track signal to fill it; if not found, the bin will be empty (0 amplitude).
  If more than one track fits a certain bin, the one with highest amplitude will be chosen. This
  means that not all of the input signal is actually 'binned', the operation is lossy. However, in
  many situations this loss is not perceptually relevant.

> fsig  binit  fin, isize

csound doc: <https://csound.com/docs/manual/binit.html>
-}
binit :: Spec -> D -> Spec
binit b1 b2 =
  Spec $ f <$> unSpec b1 <*> unD b2
  where
    f a1 a2 = opcs "binit" [(Fr, [Fr, Ir])] [a1, a2]

{- |
Generate an fsig from a mono audio source ain, using phase
      vocoder overlap-add analysis and GPU hardware. Experimental and
      only available as source code at the moment.

Generate an fsig from a mono audio source ain, using phase vocoder overlap-add analysis and GPU hardware.

> fsig  cudanal  ain, ifftsize, ioverlap, iwinsize, iwintype [, iformat] [, iinit]

csound doc: <https://csound.com/docs/manual/cudanal.html>
-}
cudanal :: Sig -> D -> D -> D -> D -> Spec
cudanal b1 b2 b3 b4 b5 =
  Spec $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5
  where
    f a1 a2 a3 a4 a5 = opcs "cudanal" [(Fr, [Ar, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
Perform sliding phase vocoder algorithm with simplified
      transformational FM using GPU hardware. Experimental and
      only available as source code at the moment.

Perform sliding phase vocoder algorithm with simplified
      transformational FM using GPU hardware.

> asig  cudasliding  ain, amod, iwinsize

csound doc: <https://csound.com/docs/manual/cudasliding.html>
-}
cudasliding :: Sig -> Sig -> D -> Sig
cudasliding b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "cudasliding" [(Ar, [Ar, Ar, Ir])] [a1, a2, a3]

{- |
Synthesis by additive synthesis and inverse FFT. Experimental and
      only available as source code at the moment.

Synthesis by additive synthesis and inverse FFT.

> asig  cudasynth  kamp, kfreq, itab, iftab, iatab[, inum]
> asig  cudasynth  fsig, kamp, kfreq[, inum]
> asig  cudasynth  fsig

csound doc: <https://csound.com/docs/manual/cudasynth.html>
-}
cudasynth :: Sig -> Sig -> Tab -> D -> D -> Sig
cudasynth b1 b2 b3 b4 b5 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unTab b3 <*> unD b4 <*> unD b5
  where
    f a1 a2 a3 a4 a5 =
      opcs
        "cudasynth"
        [(Ar, [Kr, Kr, Ir, Ir, Ir, Ir]), (Ar, [Fr, Kr, Kr, Ir]), (Ar, [Fr])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        ]

--
-- >  part2txt  SFile,ftrks
--
-- csound doc: <https://csound.com/docs/manual/part2txt.html>
part2txt :: Str -> Spec -> SE ()
part2txt b1 b2 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unSpec) b2
  where
    f a1 a2 = opcsDep_ "part2txt" [(Xr, [Sr, Fr])] [a1, a2]

{- |
Partial track spectral analysis.

The partials opcode takes two input PV streaming signals containg AMP_FREQ and AMP_PHASE signals (as generated
  for instance by pvsifd or in the first case, by pvsanal) and performs partial track analysis,
  as described in Lazzarini et al, "Time-stretching using the Instantaneous Frequency Distribution and Partial
  Tracking", Proc.of ICMC05, Barcelona. It generates a TRACKS PV streaming signal, containing amplitude, frequency,
  phase and track ID for each output track. This type of signal will contain a variable number of output tracks,
  up to the total number of analysis bins contained in the inputs (fftsize/2 + 1 bins). The second input (AMP_PHASE)
  is optional, as it can take the same signal as the first input. In this case, however, all phase information will
  be NULL and resynthesis using phase information cannot be performed.

> ftrks  partials  ffr, fphs, kthresh, kminpts, kmaxgap, imaxtracks

csound doc: <https://csound.com/docs/manual/partials.html>
-}
partials :: Spec -> Spec -> Sig -> Sig -> Sig -> D -> Spec
partials b1 b2 b3 b4 b5 b6 =
  Spec $ f <$> unSpec b1 <*> unSpec b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unD b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "partials" [(Fr, [Fr, Fr, Kr, Kr, Kr, Ir])] [a1, a2, a3, a4, a5, a6]

--
-- > kframe  pvs2array  kvar[], fsig
-- > kframe  pvs2array  kmags[], kfreqs[], fsig
--
-- csound doc: <https://csound.com/docs/manual/pvs2array.html>
pvs2array :: Sig -> Sig
pvs2array b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "pvs2array" [(Kr, [Kr, Fr]), (Kr, [Kr, Kr, Fr])] [a1]

{- |
Resynthesize using a fast oscillator-bank.

> ares  pvsadsyn  fsrc, inoscs, kfmod [, ibinoffset] [, ibinincr] [, iinit]

csound doc: <https://csound.com/docs/manual/pvsadsyn.html>
-}
pvsadsyn :: Spec -> D -> Sig -> Sig
pvsadsyn b1 b2 b3 =
  Sig $ f <$> unSpec b1 <*> unD b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "pvsadsyn" [(Ar, [Fr, Ir, Kr, Ir, Ir, Ir])] [a1, a2, a3]

{- |
Generate an fsig from a mono audio source ain, using phase vocoder overlap-add analysis.

> fsig  pvsanal  ain, ifftsize, ioverlap, iwinsize, iwintype [, iformat] [, iinit]

csound doc: <https://csound.com/docs/manual/pvsanal.html>
-}
pvsanal :: Sig -> D -> D -> D -> D -> Spec
pvsanal b1 b2 b3 b4 b5 =
  Spec $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5
  where
    f a1 a2 a3 a4 a5 = opcs "pvsanal" [(Fr, [Ar, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
Arpeggiate the spectral components of a streaming pv signal.

This opcode arpeggiates spectral components, by amplifying one bin and attenuating
    all the others around it. Used with an LFO it will provide a spectral arpeggiator similar to Trevor Wishart's
    CDP program specarp.

> fsig  pvsarp  fsigin, kbin, kdepth, kgain

csound doc: <https://csound.com/docs/manual/pvsarp.html>
-}
pvsarp :: Spec -> Sig -> Sig -> Sig -> Spec
pvsarp b1 b2 b3 b4 =
  Spec $ f <$> unSpec b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
  where
    f a1 a2 a3 a4 = opcs "pvsarp" [(Fr, [Fr, Kr, Kr, Kr])] [a1, a2, a3, a4]

{- |
A band pass filter working in the spectral domain.

Filter the pvoc frames, passing bins whose frequency is within a
      band, and with linear interpolation for transitional bands.

> fsig  pvsbandp  fsigin, xlowcut, xlowfull, \
>           xhighfull, xhighcut[, ktype]

csound doc: <https://csound.com/docs/manual/pvsbandp.html>
-}
pvsbandp :: Spec -> Sig -> Sig -> Sig -> Sig -> Spec
pvsbandp b1 b2 b3 b4 b5 =
  Spec $ f <$> unSpec b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5
  where
    f a1 a2 a3 a4 a5 = opcs "pvsbandp" [(Fr, [Fr, Xr, Xr, Xr, Xr, Kr])] [a1, a2, a3, a4, a5]

{- |
A band reject filter working in the spectral domain.

Filter the pvoc frames, rejecting bins whose frequency is within a
      band, and with linear interpolation for transitional bands.

> fsig  pvsbandr  fsigin, xlowcut, xlowfull, \
>           xhighfull, xhighcut[, ktype]

csound doc: <https://csound.com/docs/manual/pvsbandr.html>
-}
pvsbandr :: Spec -> Sig -> Sig -> Sig -> Sig -> Spec
pvsbandr b1 b2 b3 b4 b5 =
  Spec $ f <$> unSpec b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5
  where
    f a1 a2 a3 a4 a5 = opcs "pvsbandr" [(Fr, [Fr, Xr, Xr, Xr, Xr, Kr])] [a1, a2, a3, a4, a5]

--
-- > kbnd  pvsbandwidth  fsig
--
-- csound doc: <https://csound.com/docs/manual/pvsbandwidth.html>
pvsbandwidth :: Spec -> Sig
pvsbandwidth b1 =
  Sig $ f <$> unSpec b1
  where
    f a1 = opcs "pvsbandwidth" [(Kr, [Fr])] [a1]

{- |
Obtain the amp and freq values off a PVS signal bin.

Obtain the amp and freq values off a PVS signal bin as k-rate variables.

> kamp, kfr  pvsbin  fsig, kbin

csound doc: <https://csound.com/docs/manual/pvsbin.html>
-}
pvsbin :: Spec -> Sig -> (Sig, Sig)
pvsbin b1 b2 =
  pureTuple $ f <$> unSpec b1 <*> unSig b2
  where
    f a1 a2 = mopcs "pvsbin" ([Kr, Kr], [Fr, Kr]) [a1, a2]

{- |
Average the amp/freq time functions of each analysis channel for
    a specified time.

Average the amp/freq time functions of each analysis channel for
    a specified time (truncated to number of frames). As a side-effect
    the input pvoc stream will be delayed by that amount.

> fsig  pvsblur  fsigin, kblurtime, imaxdel

csound doc: <https://csound.com/docs/manual/pvsblur.html>
-}
pvsblur :: Spec -> Sig -> D -> Spec
pvsblur b1 b2 b3 =
  Spec $ f <$> unSpec b1 <*> unSig b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "pvsblur" [(Fr, [Fr, Kr, Ir])] [a1, a2, a3]

{- |
This opcode creates and writes to a circular buffer for f-signals (streaming PV signals).

This opcode sets up and writes to a circular buffer of length ilen (secs),
        giving a handle for the buffer and a time pointer, which holds the
        current write position (also in seconds). It can be used with one or
        more pvsbufread opcodes. Writing is circular, wrapping around at the
        end of the buffer.

> ihandle, ktime   pvsbuffer  fsig, ilen

csound doc: <https://csound.com/docs/manual/pvsbuffer.html>
-}
pvsbuffer :: Spec -> D -> (D, Sig)
pvsbuffer b1 b2 =
  pureTuple $ f <$> unSpec b1 <*> unD b2
  where
    f a1 a2 = mopcs "pvsbuffer" ([Ir, Kr], [Fr, Ir]) [a1, a2]

{- |
This opcode reads a circular buffer of f-signals (streaming PV signals).

This opcode reads from a circular buffer of length ilen (secs),
      taking a handle for the buffer and a time pointer, which holds the
      current read position (also in seconds). It is used in conjunction with a
      pvsbuffer opocde.
      Reading is circular, wrapping around at the end of the buffer.

> fsig  pvsbufread   ktime, khandle[, ilo, ihi, iclear]

csound doc: <https://csound.com/docs/manual/pvsbufread.html>
-}
pvsbufread :: Sig -> Sig -> Spec
pvsbufread b1 b2 =
  Spec $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = opcs "pvsbufread" [(Fr, [Kr, Kr, Ir, Ir, Ir])] [a1, a2]

{- |
This opcode reads a circular buffer of f-signals (streaming PV signals), with binwise additional delays.

This opcode reads from a circular buffer of length ilen (secs),
      taking a handle for the buffer and a time pointer, which holds the
      current read position (also in seconds). It is used in conjunction with a pvsbuffer opocde.
      Reading is circular, wrapping around at the end of the buffer. Extra delay times are taken from
      a function table, with each point on it defining a delay time in seconds affecting the corresponding bin.

> fsig  pvsbufread2   ktime, khandle, ift1, ift2

csound doc: <https://csound.com/docs/manual/pvsbufread2.html>
-}
pvsbufread2 :: Sig -> Sig -> D -> D -> Spec
pvsbufread2 b1 b2 b3 b4 =
  Spec $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "pvsbufread2" [(Fr, [Kr, Kr, Ir, Ir])] [a1, a2, a3, a4]

{- |
Scale the frequency components of a pv stream.

Scale the frequency components of a pv stream, resulting
      in pitch shift. Output amplitudes can be optionally modified in order
      to attempt formant preservation.

> fsig  pvscale  fsigin, kscal[, kkeepform, kgain, kcoefs]

csound doc: <https://csound.com/docs/manual/pvscale.html>
-}
pvscale :: Spec -> Sig -> Spec
pvscale b1 b2 =
  Spec $ f <$> unSpec b1 <*> unSig b2
  where
    f a1 a2 = opcs "pvscale" [(Fr, [Fr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
Calculate the spectral centroid of a signal.

Calculate the spectral centroid of a signal from its discrete Fourier transform.

> kcent  pvscent  fsig
> acent  pvscent  fsig

csound doc: <https://csound.com/docs/manual/pvscent.html>
-}
pvscent :: Spec -> Sig
pvscent b1 =
  Sig $ f <$> unSpec b1
  where
    f a1 = opcs "pvscent" [(Kr, [Fr]), (Ar, [Fr])] [a1]

{- |
Calculate the cepstrum of a pvs input, optionally liftering coefficients.
-}

--
-- > keps[]  pvsceps  fsig[, icoefs]
--
-- csound doc: <https://csound.com/docs/manual/pvsceps.html>
pvsceps :: Spec -> Sig
pvsceps b1 =
  Sig $ f <$> unSpec b1
  where
    f a1 = opcs "pvsceps" [(Kr, [Fr, Ir])] [a1]

{- |
Performs cross-synthesis between two source fsigs.

> fsig  pvscross  fsrc, fdest, kamp1, kamp2

csound doc: <https://csound.com/docs/manual/pvscross.html>
-}
pvscross :: Spec -> Spec -> Sig -> Sig -> Spec
pvscross b1 b2 b3 b4 =
  Spec $ f <$> unSpec b1 <*> unSpec b2 <*> unSig b3 <*> unSig b4
  where
    f a1 a2 a3 a4 = opcs "pvscross" [(Fr, [Fr, Fr, Kr, Kr])] [a1, a2, a3, a4]

{- |
Spectral azimuth-based de-mixing of stereo sources.

Spectral azimuth-based de-mixing of stereo sources, with a reverse-panning result. This
      opcode implements the Azimuth Discrimination and Resynthesis (ADRess) algorithm, developed by
      Dan Barry (Barry et Al. "Sound Source Separation Azimuth Discrimination and Resynthesis". DAFx'04,
      Univ. of Napoli). The source separation, or de-mixing, is controlled by two parameters: an azimuth
      position (kpos) and a subspace width (kwidth). The first one is used to locate the spectral peaks of
      individual sources on a stereo mix, whereas the second widens the 'search space', including/exclufing
      the peaks around kpos. These two parameters can be used interactively to extract source sounds from
      a stereo mix. The algorithm is particularly successful with studio recordings where individual instruments
      occupy individual panning positions; it is, in fact, a reverse-panning algorithm.

> fsig  pvsdemix  fleft, fright, kpos, kwidth, ipoints

csound doc: <https://csound.com/docs/manual/pvsdemix.html>
-}
pvsdemix :: Spec -> Spec -> Sig -> Sig -> D -> Spec
pvsdemix b1 b2 b3 b4 b5 =
  Spec $ f <$> unSpec b1 <*> unSpec b2 <*> unSig b3 <*> unSig b4 <*> unD b5
  where
    f a1 a2 a3 a4 a5 = opcs "pvsdemix" [(Fr, [Fr, Fr, Kr, Kr, Ir])] [a1, a2, a3, a4, a5]

{- |
Read a selected channel from a PVOC-EX analysis file.

Create an fsig stream by reading a selected channel from a PVOC-EX analysis file, with frame interpolation.

> fsig  pvsdiskin  SFname,ktscal,kgain[,ioffset, ichan]

csound doc: <https://csound.com/docs/manual/pvsdiskin.html>
-}
pvsdiskin :: Str -> Sig -> Sig -> Spec
pvsdiskin b1 b2 b3 =
  Spec $ f <$> unStr b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "pvsdiskin" [(Fr, [Sr, Kr, Kr, Ir, Ir])] [a1, a2, a3]

{- |
Displays a PVS signal as an amplitude vs. freq graph.

This opcode will display a PVS signal fsig. Uses X11 or  FLTK windows if enabled, else
	  (or if -g flag is set)
	  displays are approximated in ASCII characters.

>  pvsdisp  fsig[, ibins, iwtflg]

csound doc: <https://csound.com/docs/manual/pvsdisp.html>
-}
pvsdisp :: Spec -> SE ()
pvsdisp b1 =
  SE $ join $ f <$> (lift . unSpec) b1
  where
    f a1 = opcsDep_ "pvsdisp" [(Xr, [Fr, Ir, Ir])] [a1]

{- |
Multiply amplitudes of a pvoc stream by those of a second
pvoc stream, with dynamic scaling.

> fsig  pvsfilter  fsigin, fsigfil, kdepth[, igain]

csound doc: <https://csound.com/docs/manual/pvsfilter.html>
-}
pvsfilter :: Spec -> Spec -> Sig -> Spec
pvsfilter b1 b2 b3 =
  Spec $ f <$> unSpec b1 <*> unSpec b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "pvsfilter" [(Fr, [Fr, Fr, Kr, Ir])] [a1, a2, a3]

{- |
Read a selected channel from a PVOC-EX analysis file.

Create an fsig stream by reading a selected channel from a PVOC-EX analysis file loaded into memory, with frame interpolation. Only format 0 files (amplitude+frequency) are currently supported. The operation of this opcode mirrors that of pvoc, but outputs an fsig instead of a resynthesized signal.

> fsig  pvsfread  ktimpt, ifn [, ichan]

csound doc: <https://csound.com/docs/manual/pvsfread.html>
-}
pvsfread :: Sig -> Tab -> Spec
pvsfread b1 b2 =
  Spec $ f <$> unSig b1 <*> unTab b2
  where
    f a1 a2 = opcs "pvsfread" [(Fr, [Kr, Ir, Ir])] [a1, a2]

{- |
Freeze the amplitude and frequency time functions of a pv stream according to a control-rate
      trigger.

This opcodes 'freezes' the evolution of pvs stream by locking into steady amplitude and/or
      frequency values for each bin. The freezing is controlled, independently for amplitudes and
      frequencies, by a control-rate trigger, which switches the freezing 'on' if equal to or above
      1 and 'off' if below 1.

> fsig  pvsfreeze  fsigin, kfreeza, kfreezf

csound doc: <https://csound.com/docs/manual/pvsfreeze.html>
-}
pvsfreeze :: Spec -> Sig -> Sig -> Spec
pvsfreeze b1 b2 b3 =
  Spec $ f <$> unSpec b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "pvsfreeze" [(Fr, [Fr, Kr, Kr])] [a1, a2, a3]

--
-- > fsig  pvsfromarray  karr[][,ihopsize, iwinsize, iwintype]
-- > fsig  pvsfromarray  kmags[], kfreqs[][,ihopsize, iwinsize, iwintype]
--
-- csound doc: <https://csound.com/docs/manual/pvsfromarray.html>
pvsfromarray :: Sig -> Spec
pvsfromarray b1 =
  Spec $ f <$> unSig b1
  where
    f a1 = opcs "pvsfromarray" [(Fr, [Kr, Ir, Ir, Ir]), (Fr, [Kr, Kr, Ir, Ir, Ir])] [a1]

{- |
Reads amplitude and/or frequency data from function tables.

>  pvsftr  fsrc, ifna [, ifnf]

csound doc: <https://csound.com/docs/manual/pvsftr.html>
-}
pvsftr :: Spec -> Tab -> SE ()
pvsftr b1 b2 =
  SE $ join $ f <$> (lift . unSpec) b1 <*> (lift . unTab) b2
  where
    f a1 a2 = opcsDep_ "pvsftr" [(Xr, [Fr, Ir, Ir])] [a1, a2]

{- |
Writes amplitude and/or frequency data to function tables.

> kflag  pvsftw  fsrc, ifna [, ifnf]

csound doc: <https://csound.com/docs/manual/pvsftw.html>
-}
pvsftw :: Spec -> Tab -> Sig
pvsftw b1 b2 =
  Sig $ f <$> unSpec b1 <*> unTab b2
  where
    f a1 a2 = opcs "pvsftw" [(Kr, [Fr, Ir, Ir])] [a1, a2]

{- |
Write a fsig to a PVOCEX file.

This opcode writes a fsig to a PVOCEX file (which in turn can be read by pvsfread or other programs that support PVOCEX file input).

>  pvsfwrite  fsig, ifile

csound doc: <https://csound.com/docs/manual/pvsfwrite.html>
-}
pvsfwrite :: Spec -> Str -> SE ()
pvsfwrite b1 b2 =
  SE $ join $ f <$> (lift . unSpec) b1 <*> (lift . unStr) b2
  where
    f a1 a2 = opcsDep_ "pvsfwrite" [(Xr, [Fr, Sr])] [a1, a2]

{- |
Scale the amplitude of a pv stream.

> fsig  pvsgain  fsigin, kgain

csound doc: <https://csound.com/docs/manual/pvsgain.html>
-}
pvsgain :: Spec -> Sig -> Spec
pvsgain b1 b2 =
  Spec $ f <$> unSpec b1 <*> unSig b2
  where
    f a1 a2 = opcs "pvsgain" [(Fr, [Fr, Kr])] [a1, a2]

{- |
Shift the frequency components of a pv stream, stretching/compressing
      its spectrum.

> fsig  pvshift  fsigin, kshift, klowest[, kkeepform, igain, kcoefs]

csound doc: <https://csound.com/docs/manual/pvshift.html>
-}
pvshift :: Spec -> Sig -> Sig -> Spec
pvshift b1 b2 b3 =
  Spec $ f <$> unSpec b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "pvshift" [(Fr, [Fr, Kr, Kr, Kr, Ir, Kr])] [a1, a2, a3]

{- |
Instantaneous Frequency Distribution, magnitude and phase analysis.

The pvsifd opcode takes an input a-rate signal and performs an Instantaneous Frequency,
  magnitude and phase analysis, using the STFT and pvsifd (Instantaneous Frequency Distribution),
  as described in Lazzarini et al, "Time-stretching using the Instantaneous Frequency Distribution and Partial
  Tracking", Proc.of ICMC05, Barcelona. It generates two PV streaming signals, one containing the
  amplitudes and frequencies (a similar output to pvsanal) and another containing amplitudes and
  unwrapped phases.

> ffr,fphs  pvsifd  ain, ifftsize, ihopsize, iwintype[,iscal]

csound doc: <https://csound.com/docs/manual/pvsifd.html>
-}
pvsifd :: Sig -> D -> D -> D -> (Spec, Spec)
pvsifd b1 b2 b3 b4 =
  pureTuple $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 = mopcs "pvsifd" ([Fr, Fr], [Ar, Ir, Ir, Ir, Ir]) [a1, a2, a3, a4]

{- |
Retrieve an fsig from the input software bus; a pvs equivalent to chani.

This opcode retrieves an f-sig from the pvs in software bus, which can be
      used to get data from an external source, using the Csound 5 API. A channel
      is created if not already existing. The fsig channel is in that case initialised with
      the given parameters. It is important to note that the pvs input
      and output (pvsout opcode) busses are independent and data is not shared between them.

> fsig  pvsin  kchan[, isize, iolap, iwinsize, iwintype, iformat]

csound doc: <https://csound.com/docs/manual/pvsin.html>
-}
pvsin :: Sig -> Spec
pvsin b1 =
  Spec $ f <$> unSig b1
  where
    f a1 = opcs "pvsin" [(Fr, [Kr, Ir, Ir, Ir, Ir, Ir])] [a1]

{- |
Get information from a PVOC-EX formatted source.

Get format information about fsrc, whether created by an opcode such as pvsanal, or obtained from a PVOCEX file by pvsfread. This information is available at init time, and can be used to set parameters for other pvs opcodes, and in particular for creating function tables (e.g. for pvsftw), or setting the number of oscillators for pvsadsyn.

> ioverlap, inumbins, iwinsize, iformat  pvsinfo  fsrc

csound doc: <https://csound.com/docs/manual/pvsinfo.html>
-}
pvsinfo :: Spec -> (D, D, D, D)
pvsinfo b1 =
  pureTuple $ f <$> unSpec b1
  where
    f a1 = mopcs "pvsinfo" ([Ir, Ir, Ir, Ir], [Fr]) [a1]

{- |
Initialise a spectral (f) variable to zero.

Performs the equivalent to an init operation on an f-variable.

> fsig  pvsinit  isize[, iolap, iwinsize, iwintype, iformat]

csound doc: <https://csound.com/docs/manual/pvsinit.html>
-}
pvsinit :: D -> Spec
pvsinit b1 =
  Spec $ f <$> unD b1
  where
    f a1 = opcs "pvsinit" [(Fr, [Ir, Ir, Ir, Ir, Ir])] [a1]

{- |
Modify amplitudes using a function table, with dynamic scaling.

Modify amplitudes of fsrc using  function table, with dynamic scaling.

> fsig  pvsmaska  fsrc, ifn, kdepth

csound doc: <https://csound.com/docs/manual/pvsmaska.html>
-}
pvsmaska :: Spec -> Tab -> Sig -> Spec
pvsmaska b1 b2 b3 =
  Spec $ f <$> unSpec b1 <*> unTab b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "pvsmaska" [(Fr, [Fr, Ir, Kr])] [a1, a2, a3]

{- |
Mix 'seamlessly' two pv signals.

Mix 'seamlessly' two pv signals.  This opcode combines the
      most prominent components of two pvoc streams into a single
      mixed stream.

> fsig  pvsmix  fsigin1, fsigin2

csound doc: <https://csound.com/docs/manual/pvsmix.html>
-}
pvsmix :: Spec -> Spec -> Spec
pvsmix b1 b2 =
  Spec $ f <$> unSpec b1 <*> unSpec b2
  where
    f a1 a2 = opcs "pvsmix" [(Fr, [Fr, Fr])] [a1, a2]

{- |
Smooth the amplitude and frequency time functions of a pv stream using parallel 1st order
      lowpass IIR filters with time-varying cutoff frequency.

Smooth the amplitude and frequency time functions of a pv stream using a 1st order
      lowpass IIR with time-varying cutoff frequency. This opcode uses the same filter
      as the tone opcode, but this time acting separately on the amplitude and frequency
      time functions that make up a pv stream. The cutoff frequency parameter runs at the
      control-rate, but unlike tone and tonek, it is not specified in Hz, but as fractions
      of 1/2 frame-rate (actually the pv stream sampling rate), which is easier to
      understand. This means that the highest cutoff frequency is 1 and the lowest 0; the lower
      the frequency the smoother the functions and more pronounced the effect will be.

> fsig  pvsmooth  fsigin, kacf, kfcf

csound doc: <https://csound.com/docs/manual/pvsmooth.html>
-}
pvsmooth :: Spec -> Sig -> Sig -> Spec
pvsmooth b1 b2 b3 =
  Spec $ f <$> unSpec b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "pvsmooth" [(Fr, [Fr, Kr, Kr])] [a1, a2, a3]

{- |
Performs morphing (or interpolation) between two source fsigs.

Performs morphing (or interpolation)  between two source fsigs.

> fsig  pvsmorph  fsig1, fsig2, kampint, kfrqint

csound doc: <https://csound.com/docs/manual/pvsmorph.html>
-}
pvsmorph :: Spec -> Spec -> Sig -> Sig -> Spec
pvsmorph b1 b2 b3 b4 =
  Spec $ f <$> unSpec b1 <*> unSpec b2 <*> unSig b3 <*> unSig b4
  where
    f a1 a2 a3 a4 = opcs "pvsmorph" [(Fr, [Fr, Fr, Kr, Kr])] [a1, a2, a3, a4]

{- |
PVS-based oscillator simulator.

Generates periodic signal spectra in AMP-FREQ format, with the option of four wave types:

> fsig  pvsosc  kamp, kfreq, ktype, isize [,ioverlap] [, iwinsize] [, iwintype] [, iformat]

csound doc: <https://csound.com/docs/manual/pvsosc.html>
-}
pvsosc :: Sig -> Sig -> Sig -> D -> Spec
pvsosc b1 b2 b3 b4 =
  Spec $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "pvsosc" [(Fr, [Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
Write a fsig to the pvs output bus.

This opcode writes a fsig to a channel of the pvs output bus. Note that the pvs out bus and
	    the pvs in bus are separate and independent. A new channel is created if non-existent.

>  pvsout  fsig, kchan

csound doc: <https://csound.com/docs/manual/pvsout.html>
-}
pvsout :: Spec -> Sig -> SE ()
pvsout b1 b2 =
  SE $ join $ f <$> (lift . unSpec) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "pvsout" [(Xr, [Fr, Kr])] [a1, a2]

{- |
Track the pitch and amplitude of a PVS signal.

Track the pitch and amplitude of a PVS signal as k-rate variables.

> kfr, kamp  pvspitch  fsig, kthresh

csound doc: <https://csound.com/docs/manual/pvspitch.html>
-}
pvspitch :: Spec -> Sig -> (Sig, Sig)
pvspitch b1 b2 =
  pureTuple $ f <$> unSpec b1 <*> unSig b2
  where
    f a1 a2 = mopcs "pvspitch" ([Kr, Kr], [Fr, Kr]) [a1, a2]

{- |
Phase vocoder analysis processing with onset detection/processing.

pvstanal implements phase vocoder analysis by reading function tables
containing sampled-sound sources, with GEN01, and
pvstanal will accept deferred allocation tables.

> fsig  pvstanal  ktimescal, kamp, kpitch, ktab, [kdetect, kwrap, ioffset,ifftsize, ihop, idbthresh]
>

csound doc: <https://csound.com/docs/manual/pvstanal.html>
-}
pvstanal :: Sig -> Sig -> Sig -> Tab -> Spec
pvstanal b1 b2 b3 b4 =
  Spec $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unTab b4
  where
    f a1 a2 a3 a4 = opcs "pvstanal" [(Fr, [Kr, Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
Transforms a pvoc stream according to a masking function table.

Transforms a pvoc stream according to a masking function table;
      if the pvoc stream amplitude falls below the value of the function
      for a specific pvoc channel, it applies a gain to that channel.

> fsig  pvstencil  fsigin, kgain, klevel, iftable

csound doc: <https://csound.com/docs/manual/pvstencil.html>
-}
pvstencil :: Spec -> Sig -> Sig -> D -> Spec
pvstencil b1 b2 b3 b4 =
  Spec $ f <$> unSpec b1 <*> unSig b2 <*> unSig b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "pvstencil" [(Fr, [Fr, Kr, Kr, Ir])] [a1, a2, a3, a4]

{- |
Retain only the N loudest bins.

Process a PV stream by retaining only the N bins with the
      highest amplitude, zeroing the others.

> fsig  pvstrace  fsigin, kn
> fsig, kBins[]  pvstrace  fsigin, kn[,
>          isort, imin, imax]

csound doc: <https://csound.com/docs/manual/pvstrace.html>
-}
pvstrace :: forall a. (Tuple a) => Spec -> Sig -> a
pvstrace b1 b2 =
  pureTuple $ f <$> unSpec b1 <*> unSig b2
  where
    f a1 a2 = mopcs "pvstrace" ([Fr, Kr], [Fr, Kr, Ir, Ir, Ir]) [a1, a2]

{- |
Combine the spectral envelope of one fsig with the excitation (frequencies) of another.

This opcode provides support for cross-synthesis of amplitudes and frequencies. It takes
    the amplitudes of one input fsig and combines with frequencies from another. It is a spectral
    version of the well-known channel vocoder.

> fsig  pvsvoc  famp, fexc, kdepth, kgain [,kcoefs]

csound doc: <https://csound.com/docs/manual/pvsvoc.html>
-}
pvsvoc :: Spec -> Spec -> Sig -> Sig -> Spec
pvsvoc b1 b2 b3 b4 =
  Spec $ f <$> unSpec b1 <*> unSpec b2 <*> unSig b3 <*> unSig b4
  where
    f a1 a2 a3 a4 = opcs "pvsvoc" [(Fr, [Fr, Fr, Kr, Kr, Kr])] [a1, a2, a3, a4]

{- |
Warp the spectral envelope of a PVS signal

Warp the spectral envelope of a PVS signal by means of shifting and scaling.

> fsig  pvswarp  fsigin, kscal, kshift[, klowest, kmeth, kgain, kcoefs]

csound doc: <https://csound.com/docs/manual/pvswarp.html>
-}
pvswarp :: Spec -> Sig -> Sig -> Spec
pvswarp b1 b2 b3 =
  Spec $ f <$> unSpec b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "pvswarp" [(Fr, [Fr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2, a3]

{- |
Resynthesise using a FFT overlap-add.

Resynthesise phase vocoder data (f-signal) using a FFT overlap-add.

> ares  pvsynth  fsrc, [iinit]

csound doc: <https://csound.com/docs/manual/pvsynth.html>
-}
pvsynth :: Spec -> Sig
pvsynth b1 =
  Sig $ f <$> unSpec b1
  where
    f a1 = opcs "pvsynth" [(Ar, [Fr, Ir])] [a1]

{- |
Streaming partial track additive synthesis with cubic phase interpolation with
pitch control and support for timescale-modified input

The resyn opcode takes an input containg a TRACKS pv streaming signal (as generated,
  for instance by partials). It resynthesises the signal using linear amplitude and cubic phase
  interpolation to drive a bank of interpolating oscillators with amplitude and pitch scaling controls. Resyn is
  a modified version of sinsyn, allowing for the resynthesis of data with pitch and timescale changes.

> asig  resyn  fin, kscal, kpitch, kmaxtracks, ifn

csound doc: <https://csound.com/docs/manual/resyn.html>
-}
resyn :: Spec -> Sig -> Sig -> Sig -> Tab -> Sig
resyn b1 b2 b3 b4 b5 =
  Sig $ f <$> unSpec b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unTab b5
  where
    f a1 a2 a3 a4 a5 = opcs "resyn" [(Ar, [Fr, Kr, Kr, Kr, Ir])] [a1, a2, a3, a4, a5]

{- |
Streaming partial track additive synthesis with cubic phase interpolation

The sinsyn opcode takes an input containg a TRACKS pv streaming signal (as generated,
  for instance by the partials opcode). It resynthesises the signal using linear amplitude and cubic phase
  interpolation to drive a bank of interpolating oscillators with amplitude scaling control. sinsyn
  attempts to preserve the phase of the partials in the original signal and in so doing it does not allow for
  pitch or timescale modifications of the signal.

> asig  sinsyn  fin, kscal, kmaxtracks, ifn

csound doc: <https://csound.com/docs/manual/sinsyn.html>
-}
sinsyn :: Spec -> Sig -> Sig -> Tab -> Sig
sinsyn b1 b2 b3 b4 =
  Sig $ f <$> unSpec b1 <*> unSig b2 <*> unSig b3 <*> unTab b4
  where
    f a1 a2 a3 a4 = opcs "sinsyn" [(Ar, [Fr, Kr, Kr, Ir])] [a1, a2, a3, a4]

{- |
Instantaneous Frequency Distribution, magnitude and phase analysis.

The tabifd opcode takes an input function table and performs an Instantaneous Frequency,
  magnitude and phase analysis, using the STFT and tabifd (Instantaneous Frequency Distribution),
  as described in Lazzarini et al, "Time-stretching using the Instantaneous Frequency Distribution and Partial
  Tracking", Proc.of ICMC05, Barcelona. It generates two PV streaming signals, one containing the
  amplitudes and frequencies (a similar output to pvsanal) and another containing amplitudes and
  unwrapped phases.

> ffr,fphs  tabifd  ktimpt, kamp, kpitch, ifftsize, ihopsize, iwintype,ifn

csound doc: <https://csound.com/docs/manual/tabifd.html>
-}
tabifd :: Sig -> Sig -> Sig -> D -> D -> D -> Tab -> (Spec, Spec)
tabifd b1 b2 b3 b4 b5 b6 b7 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4 <*> unD b5 <*> unD b6 <*> unTab b7
  where
    f a1 a2 a3 a4 a5 a6 a7 = mopcs "tabifd" ([Fr, Fr], [Kr, Kr, Kr, Ir, Ir, Ir, Ir]) [a1, a2, a3, a4, a5, a6, a7]

{- |
Streaming partial track additive synthesis

The tradsyn opcode takes an input containg a TRACKS pv streaming signal (as generated,
      for instance by partials),as described in Lazzarini et al, "Time-stretching using the Instantaneous Frequency Distribution and Partial
      Tracking", Proc.of ICMC05, Barcelona. It resynthesises the signal using linear amplitude and frequency
      interpolation to drive a bank of interpolating oscillators with amplitude and pitch scaling controls.

> asig  tradsyn  fin, kscal, kpitch, kmaxtracks, ifn

csound doc: <https://csound.com/docs/manual/tradsyn.html>
-}
tradsyn :: Spec -> Sig -> Sig -> Sig -> Tab -> Sig
tradsyn b1 b2 b3 b4 b5 =
  Sig $ f <$> unSpec b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unTab b5
  where
    f a1 a2 a3 a4 a5 = opcs "tradsyn" [(Ar, [Fr, Kr, Kr, Kr, Ir])] [a1, a2, a3, a4, a5]

{- |
Streaming partial track cross-synthesis.

The trcross opcode takes two inputs containg TRACKS pv streaming signals (as generated,
      for instance by partials) and cross-synthesises them into a single TRACKS stream. Two
      different modes of operation are used: mode 0, cross-synthesis by multiplication of
      the amplitudes of the two inputs and mode 1, cross-synthesis by the substititution of
      the amplitudes of input 1 by the input 2. Frequencies and phases of input 1 are preserved
      in the output. The cross-synthesis is done by matching tracks between the two inputs using
      a 'search interval'. The matching algorithm will look for tracks in the second input that
      are within the search interval around each track in the first input. This interval can be changed
      at the control rate. Wider search intervals will find more matches.

> fsig  trcross  fin1, fin2, ksearch, kdepth [, kmode]

csound doc: <https://csound.com/docs/manual/trcross.html>
-}
trcross :: Spec -> Spec -> Sig -> Sig -> Spec
trcross b1 b2 b3 b4 =
  Spec $ f <$> unSpec b1 <*> unSpec b2 <*> unSig b3 <*> unSig b4
  where
    f a1 a2 a3 a4 = opcs "trcross" [(Fr, [Fr, Fr, Kr, Kr, Kr])] [a1, a2, a3, a4]

{- |
Streaming partial track filtering.

The trfilter opcode takes an input containg a TRACKS pv streaming signal (as generated,
      for instance by partials) and filters it using an amplitude response curve stored in
      a function table. The function table can have any size (no restriction to powers-of-two).
      The table lookup is done by linear-interpolation. It is possible to create time-varying
      filter curves by updating the amlitude response table with a table-writing opcode.

> fsig  trfilter  fin, kamnt, ifn

csound doc: <https://csound.com/docs/manual/trfilter.html>
-}
trfilter :: Spec -> Sig -> Tab -> Spec
trfilter b1 b2 b3 =
  Spec $ f <$> unSpec b1 <*> unSig b2 <*> unTab b3
  where
    f a1 a2 a3 = opcs "trfilter" [(Fr, [Fr, Kr, Ir])] [a1, a2, a3]

{- |
Extracts the highest-frequency track from a streaming track input signal.

The trhighest opcode takes an input containg TRACKS pv streaming signals (as generated,
      for instance by partials) and outputs only the highest track. In addition it outputs
      two k-rate signals, corresponding to the frequency and amplitude of the highest track
      signal.

> fsig, kfr, kamp  trhighest  fin1, kscal

csound doc: <https://csound.com/docs/manual/trhighest.html>
-}
trhighest :: Spec -> Sig -> (Spec, Sig, Sig)
trhighest b1 b2 =
  pureTuple $ f <$> unSpec b1 <*> unSig b2
  where
    f a1 a2 = mopcs "trhighest" ([Fr, Kr, Kr], [Fr, Kr]) [a1, a2]

{- |
Extracts the lowest-frequency track from a streaming track input signal.

The trlowest opcode takes an input containg TRACKS pv streaming signals (as generated,
      for instance by partials) and outputs only the lowest track. In addition it outputs
      two k-rate signals, corresponding to the frequency and amplitude of the lowest track
      signal.

> fsig, kfr, kamp  trlowest  fin1, kscal

csound doc: <https://csound.com/docs/manual/trlowest.html>
-}
trlowest :: Spec -> Sig -> (Spec, Sig, Sig)
trlowest b1 b2 =
  pureTuple $ f <$> unSpec b1 <*> unSig b2
  where
    f a1 a2 = mopcs "trlowest" ([Fr, Kr, Kr], [Fr, Kr]) [a1, a2]

{- |
Streaming partial track mixing.

The trmix opcode takes two inputs containg TRACKS pv streaming signals (as generated,
      for instance by partials) and mixes them into a single TRACKS stream. Tracks will be
      mixed up to the available space (defined by the original number of FFT bins in
      the analysed signals). If the sum of the input tracks exceeds this space, the higher-ordered
      tracks in the second input will be pruned.

> fsig  trmix  fin1, fin2

csound doc: <https://csound.com/docs/manual/trmix.html>
-}
trmix :: Spec -> Spec -> Spec
trmix b1 b2 =
  Spec $ f <$> unSpec b1 <*> unSpec b2
  where
    f a1 a2 = opcs "trmix" [(Fr, [Fr, Fr])] [a1, a2]

{- |
Streaming partial track frequency scaling.

The trscale opcode takes an input containg a TRACKS pv streaming signal (as generated,
      for instance by partials) and scales all frequencies by a k-rate amount. It can also, optionally,
      scale the gain of the signal by a k-rate amount (default 1). The result is pitch shifting of
      the input tracks.

> fsig  trscale  fin, kpitch[, kgain]

csound doc: <https://csound.com/docs/manual/trscale.html>
-}
trscale :: Spec -> Sig -> Spec
trscale b1 b2 =
  Spec $ f <$> unSpec b1 <*> unSig b2
  where
    f a1 a2 = opcs "trscale" [(Fr, [Fr, Kr, Kr])] [a1, a2]

{- |
Streaming partial track frequency scaling.

The trshift opcode takes an input containg a TRACKS pv streaming signal (as generated,
      for instance by partials) and shifts all frequencies by a k-rate frequency. It can also, optionally,
      scale the gain of the signal by a k-rate amount (default 1). The result is frequency shifting of
      the input tracks.

> fsig  trshift  fin, kpshift[, kgain]

csound doc: <https://csound.com/docs/manual/trshift.html>
-}
trshift :: Spec -> Sig -> Spec
trshift b1 b2 =
  Spec $ f <$> unSpec b1 <*> unSig b2
  where
    f a1 a2 = opcs "trshift" [(Fr, [Fr, Kr, Kr])] [a1, a2]

{- |
Streaming partial track frequency splitting.

The trsplit opcode takes an input containg a TRACKS pv streaming signal (as generated,
      for instance by partials) and splits it into two signals according to a k-rate frequency 'split point'.
      The first output will contain all tracks up from 0Hz to the split frequency and the second will
      contain the tracks from the split frequency up to the Nyquist.
      It can also, optionally, scale the gain of the output signals by a k-rate amount (default 1).
      The result is two output signals containing only part of the original spectrum.

> fsiglow, fsighi  trsplit  fin, ksplit[, kgainlow, kgainhigh]

csound doc: <https://csound.com/docs/manual/trsplit.html>
-}
trsplit :: Spec -> Sig -> (Spec, Spec)
trsplit b1 b2 =
  pureTuple $ f <$> unSpec b1 <*> unSig b2
  where
    f a1 a2 = mopcs "trsplit" ([Fr, Fr], [Fr, Kr, Kr, Kr]) [a1, a2]

-- ATS.

{- |
uses the data from an ATS analysis file to perform additive synthesis.

ATSadd reads from an ATS analysis file and uses the data to perform additive synthesis using an internal array of interpolating oscillators.

> ar  ATSadd  ktimepnt, kfmod, iatsfile, ifn, ipartials[, ipartialoffset, \
>             ipartialincr, igatefn]

csound doc: <https://csound.com/docs/manual/ATSadd.html>
-}
atsAdd :: Sig -> Sig -> D -> Tab -> D -> Sig
atsAdd b1 b2 b3 b4 b5 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unTab b4 <*> unD b5
  where
    f a1 a2 a3 a4 a5 = opcs "ATSadd" [(Ar, [Kr, Kr, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
uses the data from an ATS analysis file to perform noise resynthesis.

ATSaddnz reads from an ATS analysis file and uses the data to perform additive synthesis using a modified randi function.

> ar  ATSaddnz  ktimepnt, iatsfile, ibands[, ibandoffset, ibandincr]

csound doc: <https://csound.com/docs/manual/ATSaddnz.html>
-}
atsAddnz :: Sig -> D -> D -> Sig
atsAddnz b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "ATSaddnz" [(Ar, [Kr, Ir, Ir, Ir, Ir])] [a1, a2, a3]

{- |
reads data from and ATS data file and stores it in an internal data table of frequency, amplitude pairs.

ATSbufread reads data from and ATS data file and stores it in an internal data table of frequency, amplitude pairs.

>  ATSbufread  ktimepnt, kfmod, iatsfile, ipartials[, ipartialoffset, \
>               ipartialincr]

csound doc: <https://csound.com/docs/manual/ATSbufread.html>
-}
atsBufread :: Sig -> Sig -> D -> D -> SE ()
atsBufread b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "ATSbufread" [(Xr, [Kr, Kr, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
perform cross synthesis from ATS analysis files.

ATScross uses data from an ATS analysis file and data from an ATSbufread to perform cross synthesis.

> ar  ATScross  ktimepnt, kfmod, iatsfile, ifn, kmylev, kbuflev, ipartials \
>               [, ipartialoffset, ipartialincr]

csound doc: <https://csound.com/docs/manual/ATScross.html>
-}
atsCross :: Sig -> Sig -> D -> Tab -> Sig -> Sig -> D -> Sig
atsCross b1 b2 b3 b4 b5 b6 b7 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unTab b4 <*> unSig b5 <*> unSig b6 <*> unD b7
  where
    f a1 a2 a3 a4 a5 a6 a7 =
      opcs
        "ATScross"
        [(Ar, [Kr, Kr, Ir, Ir, Kr, Kr, Ir, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        ]

{- |
reads data out of the header of an ATS file.

atsinfo reads data out of the header of an ATS file.

> idata  ATSinfo  iatsfile, ilocation

csound doc: <https://csound.com/docs/manual/ATSinfo.html>
-}
atsInfo :: D -> D -> D
atsInfo b1 b2 =
  D $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "ATSinfo" [(Ir, [Ir, Ir])] [a1, a2]

{- |
allows a user to determine the frequency envelope of any ATSbufread.

ATSinterpread allows a user to determine the frequency envelope of any ATSbufread.

> kamp  ATSinterpread  kfreq

csound doc: <https://csound.com/docs/manual/ATSinterpread.html>
-}
atsInterpread :: Sig -> Sig
atsInterpread b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "ATSinterpread" [(Kr, [Kr])] [a1]

{- |
returns a frequency, amplitude pair from an ATSbufread opcode.

ATSpartialtap takes a partial number and returns a frequency, amplitude pair. The frequency and amplitude data comes from an ATSbufread opcode.

> kfrq, kamp  ATSpartialtap  ipartialnum

csound doc: <https://csound.com/docs/manual/ATSpartialtap.html>
-}
atsPartialtap :: D -> (Sig, Sig)
atsPartialtap b1 =
  pureTuple $ f <$> unD b1
  where
    f a1 = mopcs "ATSpartialtap" ([Kr, Kr], [Ir]) [a1]

{- |
reads data from an ATS file.

ATSread returns the amplitude (kamp) and frequency (kfreq) information of a user specified partial contained in the ATS analysis file at the time indicated by the time pointer ktimepnt.

> kfreq, kamp  ATSread  ktimepnt, iatsfile, ipartial

csound doc: <https://csound.com/docs/manual/ATSread.html>
-}
atsRead :: Sig -> D -> D -> (Sig, Sig)
atsRead b1 b2 b3 =
  pureTuple $ f <$> unSig b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = mopcs "ATSread" ([Kr, Kr], [Kr, Ir, Ir]) [a1, a2, a3]

{- |
reads data from an ATS file.

ATSreadnz returns the energy (kenergy) of a user specified noise band (1-25 bands) at the time indicated by the time pointer ktimepnt.

> kenergy  ATSreadnz  ktimepnt, iatsfile, iband

csound doc: <https://csound.com/docs/manual/ATSreadnz.html>
-}
atsReadnz :: Sig -> D -> D -> Sig
atsReadnz b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "ATSreadnz" [(Kr, [Kr, Ir, Ir])] [a1, a2, a3]

{- |
uses the data from an ATS analysis file to perform resynthesis.

ATSsinnoi reads data from an ATS data file and uses the information to synthesize sines and noise together.

> ar  ATSsinnoi  ktimepnt, ksinlev, knzlev, kfmod, iatsfile, ipartials \
>               [, ipartialoffset, ipartialincr]

csound doc: <https://csound.com/docs/manual/ATSsinnoi.html>
-}
atsSinnoi :: Sig -> Sig -> Sig -> Sig -> D -> D -> Sig
atsSinnoi b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unD b5 <*> unD b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "ATSsinnoi" [(Ar, [Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

-- Loris.

{- |
Morphs two stored sets of bandwidth-enhanced partials
    and stores a new set of partials representing the morphed
    sound. The morph is performed by linearly interpolating the
    parameter envelopes (frequency, amplitude, and bandwidth, or
    noisiness) of the bandwidth-enhanced partials according to
    control-rate frequency, amplitude, and bandwidth morphing
    functions.

lorismorph morphs two stored sets of bandwidth-enhanced
    partials and stores a new set of partials representing the morphed
    sound. The morph is performed by linearly interpolating the
    parameter envelopes (frequency, amplitude, and bandwidth, or
    noisiness) of the bandwidth-enhanced partials according to
    control-rate frequency, amplitude, and bandwidth morphing
    functions.

>  lorismorph  isrcidx, itgtidx, istoreidx, kfreqmorphenv, kampmorphenv, kbwmorphenv

csound doc: <https://csound.com/docs/manual/lorismorph.html>
-}
lorismorph :: D -> D -> D -> Sig -> Sig -> Sig -> SE ()
lorismorph b1 b2 b3 b4 b5 b6 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5 <*> (lift . unSig) b6
  where
    f a1 a2 a3 a4 a5 a6 = opcsDep_ "lorismorph" [(Xr, [Ir, Ir, Ir, Kr, Kr, Kr])] [a1, a2, a3, a4, a5, a6]

{- |
renders a stored set of bandwidth-enhanced partials using the method of Bandwidth-Enhanced Additive Synthesis implemented in the Loris software, applying control-rate frequency, amplitude, and bandwidth scaling envelopes.

lorisplay renders a stored set of
    bandwidth-enhanced partials using the method of Bandwidth-Enhanced
    Additive Synthesis implemented in the Loris software, applying
    control-rate frequency, amplitude, and bandwidth scaling
    envelopes.

> ar  lorisplay  ireadidx, kfreqenv, kampenv, kbwenv

csound doc: <https://csound.com/docs/manual/lorisplay.html>
-}
lorisplay :: D -> Sig -> Sig -> Sig -> Sig
lorisplay b1 b2 b3 b4 =
  Sig $ f <$> unD b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
  where
    f a1 a2 a3 a4 = opcs "lorisplay" [(Ar, [Ir, Kr, Kr, Kr])] [a1, a2, a3, a4]

{- |
Imports a set of bandwidth-enhanced partials from a SDIF-format
    data file, applying control-rate frequency, amplitude, and
    bandwidth scaling envelopes, and stores the modified partials in
    memory.

lorisread imports a set of bandwidth-enhanced partials from a SDIF-format data file, applying control-rate frequency, amplitude, and bandwidth scaling envelopes, and stores the modified partials in memory.

>  lorisread  ktimpnt, ifilcod, istoreidx, kfreqenv, kampenv, kbwenv[, ifadetime]

csound doc: <https://csound.com/docs/manual/lorisread.html>
-}
lorisread :: Sig -> Str -> D -> Sig -> Sig -> Sig -> SE ()
lorisread b1 b2 b3 b4 b5 b6 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unStr) b2 <*> (lift . unD) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5 <*> (lift . unSig) b6
  where
    f a1 a2 a3 a4 a5 a6 = opcsDep_ "lorisread" [(Xr, [Kr, Sr, Ir, Kr, Kr, Kr, Ir])] [a1, a2, a3, a4, a5, a6]

-- Other.

{- |
Calculate the spectral centroid of a signal.

Calculate the spectral centroid of an audio signal on a given trigger.

> kcent  centroid  asig, ktrig, ifftsize

csound doc: <https://csound.com/docs/manual/centroid.html>
-}
centroid :: Sig -> Sig -> D -> Sig
centroid b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "centroid" [(Kr, [Ar, Kr, Ir])] [a1, a2, a3]

{- |
Phase-locked vocoder processing with onset detection/processing, 'tempo-scaling'.

filescal implements phase-locked vocoder
      processing from disk files, resampling if necessary.

> asig[,asig2]  filescal  ktimescal, kamp, kpitch, Sfile, klock [,ifftsize, idecim, ithresh]
>

csound doc: <https://csound.com/docs/manual/filescal.html>
-}
filescal :: forall a. (Tuple a) => Sig -> Sig -> Sig -> Str -> Sig -> a
filescal b1 b2 b3 b4 b5 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unStr b4 <*> unSig b5
  where
    f a1 a2 a3 a4 a5 = mopcs "filescal" ([Ar, Ar], [Kr, Kr, Kr, Sr, Kr, Ir, Ir, Ir]) [a1, a2, a3, a4, a5]

{- |
Phase-locked vocoder processing.

mincer implements phase-locked vocoder processing using function tables
containing sampled-sound sources, with GEN01, and
mincer will accept deferred allocation tables.

> asig  mincer  atimpt, kamp, kpitch, ktab, klock[,ifftsize,idecim]
>

csound doc: <https://csound.com/docs/manual/mincer.html>
-}
mincer :: Sig -> Sig -> Sig -> Tab -> Sig -> Sig
mincer b1 b2 b3 b4 b5 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unTab b4 <*> unSig b5
  where
    f a1 a2 a3 a4 a5 = opcs "mincer" [(Ar, [Ar, Kr, Kr, Kr, Kr, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
Phase-locked vocoder processing with onset detection/processing, 'tempo-scaling'.

mp3scal implements phase-locked vocoder
      processing from mp3-format  disk files, resampling if necessary.

> asig, asig2, ktime  mp3scal  Sfile, ktimescal, kpitch, kamp[, iskip, ifftsize, idecim, ilock]
>

csound doc: <https://csound.com/docs/manual/mp3scal.html>
-}
mp3scal :: Str -> Sig -> Sig -> Sig -> (Sig, Sig, Sig)
mp3scal b1 b2 b3 b4 =
  pureTuple $ f <$> unStr b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
  where
    f a1 a2 a3 a4 = mopcs "mp3scal" ([Ar, Ar, Kr], [Sr, Kr, Kr, Kr, Ir, Ir, Ir, Ir]) [a1, a2, a3, a4]

{- |
Extreme time-stretching algorithm by Nasca Octavian Paul.

The paulstretch opcode is a lightweight
      implementation of the PaulStretch time-stretching algorithm by
      Nasca Octavian Paul. It is ideal for timestretching a signal by
      very large amounts.

> asig  paulstretch  istretch, iwindowsize, ift
>

csound doc: <https://csound.com/docs/manual/paulstretch.html>
-}
paulstretch :: D -> D -> D -> Sig
paulstretch b1 b2 b3 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "paulstretch" [(Ar, [Ir, Ir, Ir])] [a1, a2, a3]

{- |
Phase-locked vocoder processing with onset detection/processing, 'tempo-scaling'.

temposcal implements phase-locked vocoder processing using function tables
      containing sampled-sound sources, with GEN01, and
      temposcal will accept deferred allocation tables.

> asig  temposcal  ktimescal, kamp, kpitch, ktab, klock [,ifftsize, idecim, ithresh]
>

csound doc: <https://csound.com/docs/manual/temposcal.html>
-}
temposcal :: Sig -> Sig -> Sig -> Tab -> Sig -> Sig
temposcal b1 b2 b3 b4 b5 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unTab b4 <*> unSig b5
  where
    f a1 a2 a3 a4 a5 = opcs "temposcal" [(Ar, [Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir])] [a1, a2, a3, a4, a5]
