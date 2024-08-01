module Csound.Typed.Opcode.TableControl (
  ftfree,
  ftgen,
  ftgentmp,
  getftargs,
  sndload,

  -- * Read/Write Operations.
  ftaudio,
  ftload,
  ftloadk,
  ftprint,
  ftsamplebank,
  ftsave,
  ftsavek,
  ftset,
  ftslice,
  ftslicei,
  ptablew,
  tablecopy,
  tablefilter,
  tablefilteri,
  tablegpw,
  tableicopy,
  tableigpw,
  tableimix,
  tablemix,
  tablera,
  tablew,
  tablewa,
  tablewkt,
  tabmorph,
  tabmorpha,
  tabmorphak,
  tabmorphi,
  tabplay,
  tabrec,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Csound.Dynamic
import Csound.Typed

--

{- |
Deletes function table.

>  ftfree  ifno, iwhen

csound doc: <https://csound.com/docs/manual/ftfree.html>
-}
ftfree :: Tab -> D -> SE ()
ftfree b1 b2 =
  SE $ join $ f <$> (lift . unTab) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "ftfree" [(Xr, [Ir, Ir])] [a1, a2]

{- |
Generate a score function table from within the orchestra.

> gir  ftgen  ifn, itime, isize, igen, iarga [, iargb ] [...]
> gir  ftgen  ifn, itime, isize, igen, iarray

csound doc: <https://csound.com/docs/manual/ftgen.html>
-}
ftgen :: Tab -> D -> D -> D -> D -> SE D
ftgen b1 b2 b3 b4 b5 =
  fmap (D . return) $ SE $ join $ f <$> (lift . unTab) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep "ftgen" [(Ir, (repeat Ir)), (Ir, [Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
Generate a score function table from within the orchestra, which is deleted at the end of the note.

Generate a score function table from within the orchestra,
    which is optionally deleted at the end of the note.

> ifno  ftgentmp  ip1, ip2dummy, isize, igen, iarga, iargb, ...

csound doc: <https://csound.com/docs/manual/ftgentmp.html>
-}
ftgentmp :: D -> D -> D -> D -> D -> [D] -> SE Tab
ftgentmp b1 b2 b3 b4 b5 b6 =
  fmap (Tab . return) $ SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> mapM (lift . unD) b6
  where
    f a1 a2 a3 a4 a5 a6 = opcsDep "ftgentmp" [(Ir, (repeat Ir))] ([a1, a2, a3, a4, a5] ++ a6)

{- |
Fill a string variable with the arguments used to create a function table at k-rate.

getftargs writes the arguments used to create a function table to a string variable. getftargs runs both at initialization and performance time.

> Sdst  getftargs  iftno, ktrig

csound doc: <https://csound.com/docs/manual/getftargs.html>
-}
getftargs :: D -> Sig -> Str
getftargs b1 b2 =
  Str $ f <$> unD b1 <*> unSig b2
  where
    f a1 a2 = opcs "getftargs" [(Sr, [Ir, Kr])] [a1, a2]

{- |
Loads a sound file into memory for use by loscilx

sndload loads a sound file into memory for use by loscilx.

>  sndload  Sfname[, ifmt[, ichns[, isr[, ibas[, iamp[, istrt   \
>           [, ilpmod[, ilps[, ilpe]]]]]]]]]

csound doc: <https://csound.com/docs/manual/sndload.html>
-}
sndload :: Str -> SE ()
sndload b1 =
  SE $ join $ f <$> (lift . unStr) b1
  where
    f a1 = opcsDep_ "sndload" [(Xr, [Sr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir])] [a1]

-- Read/Write Operations.

--
-- > ians  ftaudio  ifn, "filename", iformat[, ibeg, iend]
-- > kans  ftaudio  ktrig, kfn, "filename", kformat [, isync, kbeg, kend]
--
-- csound doc: <https://csound.com/docs/manual/ftaudio.html>
ftaudio :: Tab -> Str -> D -> Sig
ftaudio b1 b2 b3 =
  Sig $ f <$> unTab b1 <*> unStr b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "ftaudio" [(Ir, [Ir, Sr, Ir, Ir, Ir]), (Kr, [Kr, Kr, Sr, Kr, Ir, Kr, Kr])] [a1, a2, a3]

--
-- >  ftload  Sfilename, iflag, ifn1 [, ifn2] [...]
--
-- csound doc: <https://csound.com/docs/manual/ftload.html>
ftload :: Str -> D -> Tab -> SE ()
ftload b1 b2 b3 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unTab) b3
  where
    f a1 a2 a3 = opcsDep_ "ftload" [(Xr, [Sr] ++ (repeat Ir))] [a1, a2, a3]

--
-- >  ftloadk  Sfilename, ktrig, iflag, ifn1 [, ifn2] [...]
--
-- csound doc: <https://csound.com/docs/manual/ftloadk.html>
ftloadk :: Str -> Sig -> D -> Tab -> SE ()
ftloadk b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unSig) b2 <*> (lift . unD) b3 <*> (lift . unTab) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "ftloadk" [(Xr, [Sr, Kr] ++ (repeat Ir))] [a1, a2, a3, a4]

--
-- >  ftprint  ifn [, ktrig, kstart, kend, kstep, inumcols ]
--
-- csound doc: <https://csound.com/docs/manual/ftprint.html>
ftprint :: Tab -> SE ()
ftprint b1 =
  SE $ join $ f <$> (lift . unTab) b1
  where
    f a1 = opcsDep_ "ftprint" [(Xr, [Ir, Kr, Kr, Kr, Kr, Ir])] [a1]

--
-- > iNumberOfFile  ftsamplebank  SDirectory, iFirstTableNumber, iSkipTime, iFormat, iChannel,
-- > kNumberOfFile  ftsamplebank  SDirectory, kFirstTableNumber, kTrigger, kSkipTime, kFormat, kChannel,
--
-- csound doc: <https://csound.com/docs/manual/ftsamplebank.html>
ftsamplebank :: Str -> D -> D -> D -> D -> Sig
ftsamplebank b1 b2 b3 b4 b5 =
  Sig $ f <$> unStr b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5
  where
    f a1 a2 a3 a4 a5 =
      opcs
        "ftsamplebank"
        [(Ir, [Sr, Ir, Ir, Ir, Ir]), (Kr, [Sr, Kr, Kr, Kr, Kr, Kr])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        ]

--
-- >  ftsave  "filename", iflag, ifn1 [, ifn2] [...]
--
-- csound doc: <https://csound.com/docs/manual/ftsave.html>
ftsave :: Str -> D -> Tab -> SE ()
ftsave b1 b2 b3 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unTab) b3
  where
    f a1 a2 a3 = opcsDep_ "ftsave" [(Xr, [Sr] ++ (repeat Ir))] [a1, a2, a3]

--
-- >  ftsavek  "filename", ktrig, iflag, ifn1 [, ifn2] [...]
--
-- csound doc: <https://csound.com/docs/manual/ftsavek.html>
ftsavek :: Str -> Sig -> D -> Tab -> SE ()
ftsavek b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unSig) b2 <*> (lift . unD) b3 <*> (lift . unTab) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "ftsavek" [(Xr, [Sr, Kr] ++ (repeat Ir))] [a1, a2, a3, a4]

--
-- >  ftset  ktablenum, kvalue [, kstart=0, kend=0, kstep=1 ]
-- >  ftset  itablenum, ivalue [, istart=0, iend=0, istep=1 ]
--
-- csound doc: <https://csound.com/docs/manual/ftset.html>
ftset :: Sig -> Sig -> SE ()
ftset b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "ftset" [(Xr, [Kr, Kr, Kr, Kr, Kr])] [a1, a2]

--
-- >  ftslice  ifnsource, ifndest [, kstart, kend, kstep ]
-- >  ftslice  kfnsource, kfndest [, kstart, kend, kstep ]
--
-- csound doc: <https://csound.com/docs/manual/ftslice.html>
ftslice :: Tab -> Tab -> SE ()
ftslice b1 b2 =
  SE $ join $ f <$> (lift . unTab) b1 <*> (lift . unTab) b2
  where
    f a1 a2 = opcsDep_ "ftslice" [(Xr, [Ir, Ir, Kr, Kr, Kr])] [a1, a2]

--
-- >  ftslicei  ifnsource, ifndest [, istart, iend, istep ]
--
-- csound doc: <https://csound.com/docs/manual/ftslicei.html>
ftslicei :: Tab -> Tab -> SE ()
ftslicei b1 b2 =
  SE $ join $ f <$> (lift . unTab) b1 <*> (lift . unTab) b2
  where
    f a1 a2 = opcsDep_ "ftslicei" [(Xr, [Ir, Ir, Ir, Ir, Ir])] [a1, a2]

--
-- >  ptablew  asig, andx, ifn [, ixmode] [, ixoff] [, iwgmode]
-- >  ptablew  isig, indx, ifn [, ixmode] [, ixoff] [, iwgmode]
-- >  ptablew  ksig, kndx, ifn [, ixmode] [, ixoff] [, iwgmode]
--
-- csound doc: <https://csound.com/docs/manual/ptablew.html>
ptablew :: Sig -> Sig -> Tab -> SE ()
ptablew b1 b2 b3 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unTab) b3
  where
    f a1 a2 a3 = opcsDep_ "ptablew" [(Xr, [Ar, Ar, Ir, Ir, Ir, Ir])] [a1, a2, a3]

--
-- >  tablecopy  kdft, ksft
--
-- csound doc: <https://csound.com/docs/manual/tablecopy.html>
tablecopy :: Sig -> Sig -> SE ()
tablecopy b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "tablecopy" [(Xr, [Kr, Kr])] [a1, a2]

--
-- > knumpassed  tablefilter  kouttable, kintatble, kmode, kparam
--
-- csound doc: <https://csound.com/docs/manual/tablefilter.html>
tablefilter :: Sig -> Sig -> Sig -> Sig -> Sig
tablefilter b1 b2 b3 b4 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
  where
    f a1 a2 a3 a4 = opcs "tablefilter" [(Kr, [Kr, Kr, Kr, Kr])] [a1, a2, a3, a4]

--
-- > inumpassed  tablefilteri  iouttable, iintatble, imode, iparam
--
-- csound doc: <https://csound.com/docs/manual/tablefilteri.html>
tablefilteri :: D -> D -> D -> D -> D
tablefilteri b1 b2 b3 b4 =
  D $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "tablefilteri" [(Ir, [Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

--
-- >  tablegpw  kfn
--
-- csound doc: <https://csound.com/docs/manual/tablegpw.html>
tablegpw :: Tab -> SE ()
tablegpw b1 =
  SE $ join $ f <$> (lift . unTab) b1
  where
    f a1 = opcsDep_ "tablegpw" [(Xr, [Kr])] [a1]

--
-- >  tableicopy  idft, isft
--
-- csound doc: <https://csound.com/docs/manual/tableicopy.html>
tableicopy :: D -> D -> SE ()
tableicopy b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "tableicopy" [(Xr, [Ir, Ir])] [a1, a2]

--
-- >  tableigpw  ifn
--
-- csound doc: <https://csound.com/docs/manual/tableigpw.html>
tableigpw :: Tab -> SE ()
tableigpw b1 =
  SE $ join $ f <$> (lift . unTab) b1
  where
    f a1 = opcsDep_ "tableigpw" [(Xr, [Ir])] [a1]

--
-- >  tableimix  idft, idoff, ilen, is1ft, is1off, is1g, is2ft, is2off, is2g
--
-- csound doc: <https://csound.com/docs/manual/tableimix.html>
tableimix :: D -> D -> D -> D -> D -> D -> D -> D -> D -> SE ()
tableimix b1 b2 b3 b4 b5 b6 b7 b8 b9 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6 <*> (lift . unD) b7 <*> (lift . unD) b8 <*> (lift . unD) b9
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
      opcsDep_
        "tableimix"
        [(Xr, [Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir])]
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

--
-- >  tablemix  kdft, kdoff, klen, ks1ft, ks1off, ks1g, ks2ft, ks2off, ks2g
--
-- csound doc: <https://csound.com/docs/manual/tablemix.html>
tablemix :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> SE ()
tablemix b1 b2 b3 b4 b5 b6 b7 b8 b9 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5 <*> (lift . unSig) b6 <*> (lift . unSig) b7 <*> (lift . unSig) b8 <*> (lift . unSig) b9
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
      opcsDep_
        "tablemix"
        [(Xr, [Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])]
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

--
-- > ares  tablera  kfn, kstart, koff
--
-- csound doc: <https://csound.com/docs/manual/tablera.html>
tablera :: Tab -> Sig -> Sig -> Sig
tablera b1 b2 b3 =
  Sig $ f <$> unTab b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "tablera" [(Ar, [Kr, Kr, Kr])] [a1, a2, a3]

--
-- >  tablew  asig, andx, ifn [, ixmode] [, ixoff] [, iwgmode]
-- >  tablew  isig, indx, ifn [, ixmode] [, ixoff] [, iwgmode]
-- >  tablew  ksig, kndx, ifn [, ixmode] [, ixoff] [, iwgmode]
--
-- csound doc: <https://csound.com/docs/manual/tablew.html>
tablew :: Sig -> Sig -> Tab -> SE ()
tablew b1 b2 b3 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unTab) b3
  where
    f a1 a2 a3 = opcsDep_ "tablew" [(Xr, [Ar, Ar, Ir, Ir, Ir, Ir])] [a1, a2, a3]

--
-- > kstart  tablewa  kfn, asig, koff
--
-- csound doc: <https://csound.com/docs/manual/tablewa.html>
tablewa :: Tab -> Sig -> Sig -> Sig
tablewa b1 b2 b3 =
  Sig $ f <$> unTab b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "tablewa" [(Kr, [Kr, Ar, Kr])] [a1, a2, a3]

--
-- >  tablewkt  asig, andx, kfn [, ixmode] [, ixoff] [, iwgmode]
-- >  tablewkt  ksig, kndx, kfn [, ixmode] [, ixoff] [, iwgmode]
--
-- csound doc: <https://csound.com/docs/manual/tablewkt.html>
tablewkt :: Sig -> Sig -> Tab -> SE ()
tablewkt b1 b2 b3 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unTab) b3
  where
    f a1 a2 a3 = opcsDep_ "tablewkt" [(Xr, [Ar, Ar, Kr, Ir, Ir, Ir])] [a1, a2, a3]

--
-- > kout  tabmorph  kindex, kweightpoint, ktabnum1, ktabnum2, \
-- >           ifn1, ifn2 [, ifn3, ifn4, ...,ifnN]
--
-- csound doc: <https://csound.com/docs/manual/tabmorph.html>
tabmorph :: Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Sig
tabmorph b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unTab b5 <*> unTab b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "tabmorph" [(Kr, [Kr, Kr, Kr, Kr] ++ (repeat Ir))] [a1, a2, a3, a4, a5, a6]

--
-- > aout  tabmorpha  aindex, aweightpoint, atabnum1, atabnum2, \
-- >           ifn1, ifn2 [, ifn3, ifn4, ... ifnN]
--
-- csound doc: <https://csound.com/docs/manual/tabmorpha.html>
tabmorpha :: Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Sig
tabmorpha b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unTab b5 <*> unTab b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "tabmorpha" [(Ar, [Ar, Ar, Ar, Ar] ++ (repeat Ir))] [a1, a2, a3, a4, a5, a6]

--
-- > aout  tabmorphak  aindex, kweightpoint, ktabnum1, ktabnum2, \
-- >           ifn1, ifn2 [, ifn3, ifn4, ... ifnN]
--
-- csound doc: <https://csound.com/docs/manual/tabmorphak.html>
tabmorphak :: Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Sig
tabmorphak b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unTab b5 <*> unTab b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "tabmorphak" [(Ar, [Ar, Kr, Kr, Kr] ++ (repeat Ir))] [a1, a2, a3, a4, a5, a6]

--
-- > kout  tabmorphi  kindex, kweightpoint, ktabnum1, ktabnum2, \
-- >           ifn1, ifn2 [, ifn3, ifn4, ..., ifnN]
--
-- csound doc: <https://csound.com/docs/manual/tabmorphi.html>
tabmorphi :: Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Sig
tabmorphi b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unTab b5 <*> unTab b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "tabmorphi" [(Kr, [Kr, Kr, Kr, Kr] ++ (repeat Ir))] [a1, a2, a3, a4, a5, a6]

--
-- >  tabplay   ktrig, knumtics, kfn, kout1 [,kout2,..., koutN]
--
-- csound doc: <https://csound.com/docs/manual/tabplay.html>
tabplay :: Sig -> Sig -> Tab -> Sig -> SE ()
tabplay b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unTab) b3 <*> (lift . unSig) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "tabplay" [(Xr, (repeat Kr))] [a1, a2, a3, a4]

--
-- >  tabrec    ktrig_start, ktrig_stop, knumtics, kfn, kin1 [,kin2,...,kinN]
--
-- csound doc: <https://csound.com/docs/manual/tabrec.html>
tabrec :: Sig -> Sig -> Sig -> Tab -> Sig -> SE ()
tabrec b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unTab) b4 <*> (lift . unSig) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "tabrec" [(Xr, (repeat Kr))] [a1, a2, a3, a4, a5]
