module Csound.Typed.Opcode.SignalIO (
  -- * File I/O.
  dumpk,
  dumpk2,
  dumpk3,
  dumpk4,
  ficlose,
  fin,
  fini,
  fink,
  fiopen,
  fout,
  fouti,
  foutir,
  foutk,
  fprintks,
  fprints,
  hdf5read,
  hdf5write,
  readf,
  readfi,
  readk,
  readk2,
  readk3,
  readk4,
  websocket,

  -- * Signal Input.
  diskin,
  diskin2,
  in',
  in32,
  inch,
  inh,
  ino,
  inq,
  inrg,
  ins,
  invalue,
  inx,
  inz,
  mp3in,
  soundin,

  -- * Signal Output.
  mdelay,
  monitor,
  out,
  out32,
  outall,
  outc,
  outch,
  outh,
  outo,
  outq,
  outq1,
  outq2,
  outq3,
  outq4,
  outrg,
  outs,
  outs1,
  outs2,
  outvalue,
  outx,
  outz,
  soundout,
  soundouts,

  -- * Software Bus.
  chani,
  chano,
  chn_k,
  chn_a,
  chn_S,
  chnclear,
  chnexport,
  chnget,
  chngetks,
  chngeti,
  chngetk,
  chngeta,
  chngets,
  chnmix,
  chnparams,
  chnset,
  chnsetks,
  chnseti,
  chnsetk,
  chnseta,
  chnsets,
  setksmps,
  xin,
  xout,

  -- * Printing and Display.
  dispfft,
  display,
  flashtxt,
  print',
  printf_i,
  printf,
  printk,
  printk2,
  printks,
  printks2,
  println,
  prints,
  printsk,

  -- * Soundfile Queries.
  filebit,
  filelen,
  filenchnls,
  filepeak,
  filesr,
  filevalid,
  mp3len,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Csound.Dynamic
import Csound.Typed

-- File I/O.

{- |
Periodically writes an orchestra control-signal value to an external file.

Periodically writes an orchestra control-signal value to a named external file in a specific format.

>  dumpk   ksig, ifilname, iformat, iprd

csound doc: <https://csound.com/docs/manual/dumpk.html>
-}
dumpk :: Sig -> Str -> D -> D -> SE ()
dumpk b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unStr) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "dumpk" [(Xr, [Kr, Sr, Ir, Ir])] [a1, a2, a3, a4]

{- |
Periodically writes two orchestra control-signal values to an external file.

Periodically writes two orchestra control-signal values to a named external file in a specific format.

>  dumpk2  ksig1, ksig2, ifilname, iformat, iprd

csound doc: <https://csound.com/docs/manual/dumpk2.html>
-}
dumpk2 :: Sig -> Sig -> Str -> D -> D -> SE ()
dumpk2 b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unStr) b3 <*> (lift . unD) b4 <*> (lift . unD) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "dumpk2" [(Xr, [Kr, Kr, Sr, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
Periodically writes three orchestra control-signal values to an external file.

Periodically writes three orchestra control-signal values to a named external file in a specific format.

>  dumpk3  ksig1, ksig2, ksig3, ifilname, iformat, iprd

csound doc: <https://csound.com/docs/manual/dumpk3.html>
-}
dumpk3 :: Sig -> Sig -> Sig -> Str -> D -> D -> SE ()
dumpk3 b1 b2 b3 b4 b5 b6 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unStr) b4 <*> (lift . unD) b5 <*> (lift . unD) b6
  where
    f a1 a2 a3 a4 a5 a6 = opcsDep_ "dumpk3" [(Xr, [Kr, Kr, Kr, Sr, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

{- |
Periodically writes four orchestra control-signal values to an external file.

Periodically writes four orchestra control-signal values to a named external file in a specific format.

>  dumpk4  ksig1, ksig2, ksig3, ksig4, ifilname, iformat, iprd

csound doc: <https://csound.com/docs/manual/dumpk4.html>
-}
dumpk4 :: Sig -> Sig -> Sig -> Sig -> Str -> D -> D -> SE ()
dumpk4 b1 b2 b3 b4 b5 b6 b7 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> (lift . unStr) b5 <*> (lift . unD) b6 <*> (lift . unD) b7
  where
    f a1 a2 a3 a4 a5 a6 a7 = opcsDep_ "dumpk4" [(Xr, [Kr, Kr, Kr, Kr, Sr, Ir, Ir])] [a1, a2, a3, a4, a5, a6, a7]

{- |
Closes a previously opened file.

ficlose can be used to close a file which was opened with fiopen.

>  ficlose  ihandle
>  ficlose  Sfilename

csound doc: <https://csound.com/docs/manual/ficlose.html>
-}
ficlose :: D -> SE ()
ficlose b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "ficlose" [(Xr, [Ir])] [a1]

{- |
Read signals from a file at a-rate.

>  fin  ifilename, iskipframes, iformat, ain1 [, ain2] [, ain3] [,...]
>  fin  ifilename, iskipframes, iformat, arr[]

csound doc: <https://csound.com/docs/manual/fin.html>
-}
fin :: Str -> D -> D -> [Sig] -> SE ()
fin b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> mapM (lift . unSig) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "fin" [(Xr, [Sr, Ir, Ir] ++ (repeat Ar))] ([a1, a2, a3] ++ a4)

{- |
Read signals from a file at i-rate.

>  fini  ifilename, iskipframes, iformat, in1 [, in2] [, in3] [, ...]

csound doc: <https://csound.com/docs/manual/fini.html>
-}
fini :: Str -> D -> D -> [D] -> SE ()
fini b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> mapM (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "fini" [(Xr, [Sr] ++ (repeat Ir))] ([a1, a2, a3] ++ a4)

{- |
Read signals from a file at k-rate.

>  fink  ifilename, iskipframes, iformat, kin1 [, kin2] [, kin3] [,...]

csound doc: <https://csound.com/docs/manual/fink.html>
-}
fink :: Str -> D -> D -> [Sig] -> SE ()
fink b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> mapM (lift . unSig) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "fink" [(Xr, [Sr, Ir, Ir] ++ (repeat Kr))] ([a1, a2, a3] ++ a4)

{- |
Opens a file in a specific mode.

fiopen can be used to open a file in one of the specified modes.

> ihandle  fiopen  ifilename, imode

csound doc: <https://csound.com/docs/manual/fiopen.html>
-}
fiopen :: Str -> D -> SE D
fiopen b1 b2 =
  fmap (D . return) $ SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep "fiopen" [(Ir, [Sr, Ir])] [a1, a2]

{- |
Outputs a-rate signals to an arbitrary number of channels.

fout outputs N a-rate signals to a specified file of N channels.

>  fout  ifilename, iformat, aout1 [, aout2, aout3,...,aoutN]
>  fout  ifilename, iformat, array[]

csound doc: <https://csound.com/docs/manual/fout.html>
-}
fout :: Str -> D -> [Sig] -> SE ()
fout b1 b2 b3 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> mapM (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep_ "fout" [(Xr, [Sr, Ir] ++ (repeat Ar))] ([a1, a2] ++ a3)

{- |
Outputs i-rate signals of an arbitrary number of channels to a specified file.

fouti output N i-rate signals to a specified file of N channels.

>  fouti  ihandle, iformat, iflag, iout1 [, iout2, iout3,....,ioutN]

csound doc: <https://csound.com/docs/manual/fouti.html>
-}
fouti :: Str -> D -> D -> [D] -> SE ()
fouti b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> mapM (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "fouti" [(Xr, (repeat Ir))] ([a1, a2, a3] ++ a4)

{- |
Outputs i-rate signals from an arbitrary number of channels to a specified file.

foutir output N i-rate signals to a specified file of N channels.

>  foutir  ihandle, iformat, iflag, iout1 [, iout2, iout3,....,ioutN]

csound doc: <https://csound.com/docs/manual/foutir.html>
-}
foutir :: Str -> D -> D -> [D] -> SE ()
foutir b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> mapM (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "foutir" [(Xr, (repeat Ir))] ([a1, a2, a3] ++ a4)

{- |
Outputs k-rate signals of an arbitrary number of channels to a specified file, in raw (headerless) format.

foutk outputs N k-rate signals to a specified file of N channels.

>  foutk  ifilename, iformat, kout1 [, kout2, kout3,....,koutN]

csound doc: <https://csound.com/docs/manual/foutk.html>
-}
foutk :: Str -> D -> [Sig] -> SE ()
foutk b1 b2 b3 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> mapM (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep_ "foutk" [(Xr, [Sr, Ir] ++ (repeat Kr))] ([a1, a2] ++ a3)

{- |
Similar to printks but prints to a file.

>  fprintks  "filename", "string", [, kval1] [, kval2] [...]

csound doc: <https://csound.com/docs/manual/fprintks.html>
-}
fprintks :: Str -> Str -> [Sig] -> SE ()
fprintks b1 b2 b3 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unStr) b2 <*> mapM (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep_ "fprintks" [(Xr, [Sr, Sr] ++ (repeat Kr))] ([a1, a2] ++ a3)

{- |
Similar to prints but prints to a file.

>  fprints  "filename", "string" [, ival1] [, ival2] [...]

csound doc: <https://csound.com/docs/manual/fprints.html>
-}
fprints :: Str -> Str -> [D] -> SE ()
fprints b1 b2 b3 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unStr) b2 <*> mapM (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "fprints" [(Xr, [Sr, Sr] ++ (repeat Ir))] ([a1, a2] ++ a3)

{- |
Read signals and arrays from an hdf5 file.

hdf5read reads N signals and arrays from a specified hdf5 file.

> xout1[, xout2, xout3, ..., xoutN]  hdf5read  ifilename, ivariablename1[, ivariablename2, ivariablename3, ..., ivariablenameN]

csound doc: <https://csound.com/docs/manual/hdf5read.html>
-}
hdf5read :: forall a. (Tuple a) => Str -> D -> a
hdf5read b1 b2 =
  pureTuple $ f <$> unStr b1 <*> unD b2
  where
    f a1 a2 = mopcs "hdf5read" ((repeat Xr), [Sr] ++ (repeat Ir)) [a1, a2]

{- |
Write signals and arrays to an hdf5 file.

hdf5write writes N signals and arrays to a specified hdf5 file.

>  hdf5write  ifilename, xout1[, xout2, xout3, ..., xoutN]

csound doc: <https://csound.com/docs/manual/hdf5write.html>
-}
hdf5write :: Str -> Sig -> SE ()
hdf5write b1 b2 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "hdf5write" [(Xr, [Sr] ++ (repeat Xr))] [a1, a2]

{- |
Read a line of text from an external file.

Read a line of text from an external file once each k-cycle.

> Sres, kline  readf  ifilname

csound doc: <https://csound.com/docs/manual/readf.html>
-}
readf :: Str -> (Str, Sig)
readf b1 =
  pureTuple $ f <$> unStr b1
  where
    f a1 = mopcs "readf" ([Sr, Kr], [Sr]) [a1]

{- |
Read a line of text from an external file.

Read a line of text from an external file once on initialisation.

> Sres, iline  readfi  ifilname

csound doc: <https://csound.com/docs/manual/readfi.html>
-}
readfi :: Str -> (Str, D)
readfi b1 =
  pureTuple $ f <$> unStr b1
  where
    f a1 = mopcs "readfi" ([Sr, Ir], [Sr]) [a1]

{- |
Periodically reads an orchestra control-signal value from an external file.

Periodically reads an orchestra control-signal value from a named external file in a specific format.

> kres  readk  ifilname, iformat, iprd

csound doc: <https://csound.com/docs/manual/readk.html>
-}
readk :: Str -> D -> D -> Sig
readk b1 b2 b3 =
  Sig $ f <$> unStr b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "readk" [(Kr, [Sr, Ir, Ir])] [a1, a2, a3]

{- |
Periodically reads two orchestra control-signal values from an external file.

> kr1, kr2  readk2  ifilname, iformat, iprd

csound doc: <https://csound.com/docs/manual/readk2.html>
-}
readk2 :: Str -> D -> D -> (Sig, Sig)
readk2 b1 b2 b3 =
  pureTuple $ f <$> unStr b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = mopcs "readk2" ([Kr, Kr], [Sr, Ir, Ir]) [a1, a2, a3]

{- |
Periodically reads three orchestra control-signal values from an external file.

> kr1, kr2, kr3  readk3  ifilname, iformat, iprd

csound doc: <https://csound.com/docs/manual/readk3.html>
-}
readk3 :: Str -> D -> D -> (Sig, Sig, Sig)
readk3 b1 b2 b3 =
  pureTuple $ f <$> unStr b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = mopcs "readk3" ([Kr, Kr, Kr], [Sr, Ir, Ir]) [a1, a2, a3]

{- |
Periodically reads four orchestra control-signal values from an external file.

> kr1, kr2, kr3, kr4  readk4  ifilname, iformat, iprd

csound doc: <https://csound.com/docs/manual/readk4.html>
-}
readk4 :: Str -> D -> D -> (Sig, Sig, Sig, Sig)
readk4 b1 b2 b3 =
  pureTuple $ f <$> unStr b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = mopcs "readk4" ([Kr, Kr, Kr, Kr], [Sr, Ir, Ir]) [a1, a2, a3]

--
-- > xout1[, xout2, xout3, ..., xoutN]  websocket  iport, xin
--
-- csound doc: <https://csound.com/docs/manual/websocket.html>
websocket :: forall a. (Tuple a) => D -> Sig -> a
websocket b1 b2 =
  pureTuple $ f <$> unD b1 <*> unSig b2
  where
    f a1 a2 = mopcs "websocket" ((repeat Xr), [Ir, Xr]) [a1, a2]

-- Signal Input.

{- |
Reads audio data from an external device or stream and can alter its pitch.

> ar1 [, ar2 [, ar3 [, ... arN]]]  diskin  ifilcod[, kpitch[, iskiptim \
>           [, iwraparound[, iformat[, iskipinit]]]]]
> ar1[]  diskin  ifilcod[, kpitch[, iskiptim \
>           [, iwraparound[, iformat[, iskipinit]]]]]

csound doc: <https://csound.com/docs/manual/diskin.html>
-}
diskin :: forall a. (Tuple a) => Str -> a
diskin b1 =
  pureTuple $ f <$> unStr b1
  where
    f a1 = mopcs "diskin" ((repeat Ar), [Sr, Kr, Ir, Ir, Ir, Ir]) [a1]

{- |
Reads audio data from a file, and can alter its pitch using one of several
      available interpolation types, as well as convert the sample rate to match
      the orchestra sr setting.

Reads audio data from a file, and can alter its pitch using
	one of several available interpolation types, as well as
	convert the sample rate to match the orchestra sr
	setting. diskin2 can also read
	multichannel files with any number of channels in the range 1
	to 24 in versions before 5.14, and 40 after.

> a1[, a2[, ... aN]]  diskin2  ifilcod[, kpitch[, iskiptim \
>           [, iwrap[, iformat[, iwsize[, ibufsize[, iskipinit]]]]]]]
> ar1[]  diskin2  ifilcod[, kpitch[, iskiptim \
>           [, iwrap[, iformat[, iwsize[, ibufsize[, iskipinit]]]]]]]

csound doc: <https://csound.com/docs/manual/diskin2.html>
-}
diskin2 :: forall a. (Tuple a) => Str -> a
diskin2 b1 =
  pureTuple $ f <$> unStr b1
  where
    f a1 = mopcs "diskin2" ((repeat Ar), [Sr, Kr, Ir, Ir, Ir, Ir, Ir, Ir]) [a1]

{- |
Reads mono audio data from an external device or stream.

Reads audio data from an external device or stream.

> ar1  in
> aarray  in

csound doc: <https://csound.com/docs/manual/in.html>
-}
in' :: Sig
in' =
  Sig $ return $ f
  where
    f = opcs "in" [(Ar, []), (Ar, [])] []

{- |
Reads a 32-channel audio signal from an external device or stream.

> ar1, ar2, ar3, ar4, ar5, ar6, ar7, ar8, ar9, ar10, ar11, ar12, ar13, ar14, \
>           ar15, ar16, ar17, ar18, ar19, ar20, ar21, ar22, ar23, ar24, ar25, ar26, \
>           ar27, ar28, ar29, ar30, ar31, ar32  in32

csound doc: <https://csound.com/docs/manual/in32.html>
-}
in32 :: forall a. (Tuple a) => a
in32 =
  pureTuple $ return $ f
  where
    f =
      mopcs
        "in32"
        (
          [ Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          ]
        , []
        )
        []

{- |
Reads from numbered channels in an external audio signal or stream.

> ain1[, ...]  inch  kchan1[,...]

csound doc: <https://csound.com/docs/manual/inch.html>
-}
inch :: forall a. (Tuple a) => [Sig] -> a
inch b1 =
  pureTuple $ f <$> mapM unSig b1
  where
    f a1 = mopcs "inch" ((repeat Ar), (repeat Kr)) a1

{- |
Reads six-channel audio data from an external device or stream.

> ar1, ar2, ar3, ar4, ar5, ar6  inh

csound doc: <https://csound.com/docs/manual/inh.html>
-}
inh :: forall a. (Tuple a) => a
inh =
  pureTuple $ return $ f
  where
    f = mopcs "inh" ([Ar, Ar, Ar, Ar, Ar, Ar], []) []

{- |
Reads eight-channel audio data from an external device or stream.

> ar1, ar2, ar3, ar4, ar5, ar6, ar7, ar8  ino

csound doc: <https://csound.com/docs/manual/ino.html>
-}
ino :: forall a. (Tuple a) => a
ino =
  pureTuple $ return $ f
  where
    f = mopcs "ino" ([Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar], []) []

{- |
Reads quad audio data from an external device or stream.

> ar1, ar2,  ar3, a4  inq

csound doc: <https://csound.com/docs/manual/inq.html>
-}
inq :: (Sig, Sig, Sig, Sig)
inq =
  pureTuple $ return $ f
  where
    f = mopcs "inq" ([Ar, Ar, Ar, Ar], []) []

{- |
Allow input from a range of adjacent audio channels from the audio input device

inrg reads audio from a range of adjacent audio channels from the audio input device.

>  inrg  kstart, ain1 [,ain2, ain3, ..., ainN]

csound doc: <https://csound.com/docs/manual/inrg.html>
-}
inrg :: Sig -> [Sig] -> SE ()
inrg b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> mapM (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "inrg" [(Xr, [Kr] ++ (repeat Ar))] ([a1] ++ a2)

{- |
Reads stereo audio data from an external device or stream.

> ar1, ar2  ins

csound doc: <https://csound.com/docs/manual/ins.html>
-}
ins :: (Sig, Sig)
ins =
  pureTuple $ return $ f
  where
    f = mopcs "ins" ([Ar, Ar], []) []

{- |
Reads a k-rate signal from a user-defined channel.

Reads a k-rate or i-rate signal or string from a user-defined channel.

> ivalue  invalue  "channel name"
> kvalue  invalue  "channel name"
> Sname  invalue  "channel name"

csound doc: <https://csound.com/docs/manual/invalue.html>
-}
invalue :: Str -> Str
invalue b1 =
  Str $ f <$> unStr b1
  where
    f a1 = opcs "invalue" [(Ir, [Sr]), (Kr, [Sr]), (Sr, [Sr])] [a1]

{- |
Reads a 16-channel audio signal from an external device or stream.

> ar1, ar2, ar3, ar4, ar5, ar6, ar7, ar8, ar9, ar10, ar11, ar12, \
>           ar13, ar14, ar15, ar16  inx

csound doc: <https://csound.com/docs/manual/inx.html>
-}
inx :: forall a. (Tuple a) => a
inx =
  pureTuple $ return $ f
  where
    f = mopcs "inx" ([Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar], []) []

{- |
Reads multi-channel audio samples into a ZAK array from an external device or stream.

>  inz  ksig1

csound doc: <https://csound.com/docs/manual/inz.html>
-}
inz :: Sig -> SE ()
inz b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "inz" [(Xr, [Kr])] [a1]

{- |
Reads mono or stereo audio data from an external MP3 file.

> ar1, ar2  mp3in  ifilcod[, iskptim, iformat, iskipinit, ibufsize]
> ar1  mp3in  ifilcod[, iskptim, iformat, iskipinit, ibufsize]

csound doc: <https://csound.com/docs/manual/mp3in.html>
-}
mp3in :: Str -> (Sig, Sig)
mp3in b1 =
  pureTuple $ f <$> unStr b1
  where
    f a1 = mopcs "mp3in" ([Ar, Ar], [Sr, Ir, Ir, Ir, Ir]) [a1]

{- |
Reads audio data from an external device or stream.

Reads audio data from an external device or stream.  Up to 24
      channels may be read before v5.14, extended to 40 in later versions.

> ar1[, ar2[, ar3[, ... a24]]]  soundin  ifilcod [, iskptim] [, iformat] \
>           [, iskipinit] [, ibufsize]

csound doc: <https://csound.com/docs/manual/soundin.html>
-}
soundin :: forall a. (Tuple a) => Str -> a
soundin b1 =
  pureTuple $ f <$> unStr b1
  where
    f a1 = mopcs "soundin" ((repeat Ar), [Sr, Ir, Ir, Ir, Ir]) [a1]

-- Signal Output.

{- |
A MIDI delay opcode.

>  mdelay  kstatus, kchan, kd1, kd2, kdelay

csound doc: <https://csound.com/docs/manual/mdelay.html>
-}
mdelay :: Sig -> Sig -> Sig -> Sig -> Sig -> SE ()
mdelay b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "mdelay" [(Xr, [Kr, Kr, Kr, Kr, Kr])] [a1, a2, a3, a4, a5]

{- |
Returns the audio spout frame.

Returns the audio spout frame (if active), otherwise it returns zero.

> aout1 [,aout2 ... aoutX]  monitor
> aarra  monitor

csound doc: <https://csound.com/docs/manual/monitor.html>
-}
monitor :: forall a. (Tuple a) => a
monitor =
  pureTuple $ return $ f
  where
    f = mopcs "monitor" ((repeat Ar), []) []

{- |
Writes audio data to an external device or stream.

Writes audio data to an external device or stream, either from
      audio variables or from an audio array.

>  out  asig1[, asig2,....]
>  out  aarray

csound doc: <https://csound.com/docs/manual/out.html>
-}
out :: Sig -> SE ()
out b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "out" [(Xr, (repeat Ar))] [a1]

{- |
Writes 32-channel audio data to an external device or stream.

>  out32  asig1, asig2, asig3, asig4, asig5, asig6, asig7, asig8, asig10, \
>           asig11, asig12, asig13, asig14, asig15, asig16, asig17, asig18, \
>           asig19, asig20, asig21, asig22, asig23, asig24, asig25, asig26, \
>           asig27, asig28, asig29, asig30, asig31, asig32

csound doc: <https://csound.com/docs/manual/out32.html>
-}
out32 :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> SE ()
out32 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5 <*> (lift . unSig) b6 <*> (lift . unSig) b7 <*> (lift . unSig) b8 <*> (lift . unSig) b9 <*> (lift . unSig) b10 <*> (lift . unSig) b11 <*> (lift . unSig) b12 <*> (lift . unSig) b13 <*> (lift . unSig) b14 <*> (lift . unSig) b15 <*> (lift . unSig) b16 <*> (lift . unSig) b17 <*> (lift . unSig) b18 <*> (lift . unSig) b19 <*> (lift . unSig) b20 <*> (lift . unSig) b21 <*> (lift . unSig) b22 <*> (lift . unSig) b23 <*> (lift . unSig) b24 <*> (lift . unSig) b25 <*> (lift . unSig) b26 <*> (lift . unSig) b27 <*> (lift . unSig) b28 <*> (lift . unSig) b29 <*> (lift . unSig) b30 <*> (lift . unSig) b31
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 =
      opcsDep_
        "out32"
        [
          ( Xr
          ,
            [ Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            , Ar
            ]
          )
        ]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        , a19
        , a20
        , a21
        , a22
        , a23
        , a24
        , a25
        , a26
        , a27
        , a28
        , a29
        , a30
        , a31
        ]

--
-- >  outall  asig
--
-- csound doc: <https://csound.com/docs/manual/outall.html>
outall :: Sig -> SE ()
outall b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "outall" [(Xr, [Ar])] [a1]

{- |
Writes audio data with an arbitrary number of channels to an external device or stream.

>  outc  asig1 [, asig2] [...]

csound doc: <https://csound.com/docs/manual/outc.html>
-}
outc :: [Sig] -> SE ()
outc b1 =
  SE $ join $ f <$> mapM (lift . unSig) b1
  where
    f a1 = opcsDep_ "outc" [(Xr, (repeat Ar))] a1

{- |
Writes multi-channel audio data, with user-controllable channels, to an external device or stream.

>  outch  kchan1, asig1 [, kchan2] [, asig2] [...]

csound doc: <https://csound.com/docs/manual/outch.html>
-}
outch :: Sig -> [Sig] -> SE ()
outch b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> mapM (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "outch" [(Xr, [Kr, Ar, Kr] ++ (repeat Ar))] ([a1] ++ a2)

{- |
Writes 6-channel audio data to an external device or stream.

>  outh  asig1, asig2, asig3, asig4, asig5, asig6

csound doc: <https://csound.com/docs/manual/outh.html>
-}
outh :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> SE ()
outh b1 b2 b3 b4 b5 b6 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5 <*> (lift . unSig) b6
  where
    f a1 a2 a3 a4 a5 a6 = opcsDep_ "outh" [(Xr, [Ar, Ar, Ar, Ar, Ar, Ar])] [a1, a2, a3, a4, a5, a6]

{- |
Writes 8-channel audio data to an external device or stream.

>  outo  asig1, asig2, asig3, asig4, asig5, asig6, asig7, asig8

csound doc: <https://csound.com/docs/manual/outo.html>
-}
outo :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> SE ()
outo b1 b2 b3 b4 b5 b6 b7 b8 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5 <*> (lift . unSig) b6 <*> (lift . unSig) b7 <*> (lift . unSig) b8
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 =
      opcsDep_
        "outo"
        [(Xr, [Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        ]

{- |
Writes 4-channel audio data to an external device or stream.

>  outq  asig1, asig2, asig3, asig4

csound doc: <https://csound.com/docs/manual/outq.html>
-}
outq :: Sig -> Sig -> Sig -> Sig -> SE ()
outq b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "outq" [(Xr, [Ar, Ar, Ar, Ar])] [a1, a2, a3, a4]

{- |
Writes samples to quad channel 1 of an external device or stream.

>  outq1  asig

csound doc: <https://csound.com/docs/manual/outq1.html>
-}
outq1 :: Sig -> SE ()
outq1 b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "outq1" [(Xr, [Ar])] [a1]

{- |
Writes samples to quad channel 2 of an external device or stream.

>  outq2  asig

csound doc: <https://csound.com/docs/manual/outq2.html>
-}
outq2 :: Sig -> SE ()
outq2 b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "outq2" [(Xr, [Ar])] [a1]

{- |
Writes samples to quad channel 3 of an external device or stream.

>  outq3  asig

csound doc: <https://csound.com/docs/manual/outq3.html>
-}
outq3 :: Sig -> SE ()
outq3 b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "outq3" [(Xr, [Ar])] [a1]

{- |
Writes samples to quad channel 4 of an external device or stream.

>  outq4  asig

csound doc: <https://csound.com/docs/manual/outq4.html>
-}
outq4 :: Sig -> SE ()
outq4 b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "outq4" [(Xr, [Ar])] [a1]

{- |
Allow output to a range of adjacent audio channels on the audio output device

outrg outputs audio to a range of adjacent audio channels on the audio output device.

>  outrg  kstart, aout1 [,aout2, aout3, ..., aoutN]

csound doc: <https://csound.com/docs/manual/outrg.html>
-}
outrg :: Sig -> [Sig] -> SE ()
outrg b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> mapM (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "outrg" [(Xr, [Kr] ++ (repeat Ar))] ([a1] ++ a2)

{- |
Writes stereo audio data to an external device or stream.

>  outs  asig1, asig2

csound doc: <https://csound.com/docs/manual/outs.html>
-}
outs :: Sig -> Sig -> SE ()
outs b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "outs" [(Xr, [Ar, Ar])] [a1, a2]

{- |
Writes samples to stereo channel 1 of an external device or stream.

>  outs1  asig

csound doc: <https://csound.com/docs/manual/outs1.html>
-}
outs1 :: Sig -> SE ()
outs1 b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "outs1" [(Xr, [Ar])] [a1]

{- |
Writes samples to stereo channel 2 of an external device or stream.

>  outs2  asig

csound doc: <https://csound.com/docs/manual/outs2.html>
-}
outs2 :: Sig -> SE ()
outs2 b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "outs2" [(Xr, [Ar])] [a1]

{- |
Sends an i-rate or k-rate signal or string to a user-defined channel.

Sends an irate or k-rate signal or string to a user-defined channel.

>  outvalue  "channel name", ivalue
>  outvalue  "channel name", kvalue
>  outvalue  "channel name", "string"

csound doc: <https://csound.com/docs/manual/outvalue.html>
-}
outvalue :: Str -> D -> SE ()
outvalue b1 b2 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "outvalue" [(Xr, [Sr, Ir])] [a1, a2]

{- |
Writes 16-channel audio data to an external device or stream.

>  outx  asig1, asig2, asig3, asig4, asig5, asig6, asig7, asig8, \
>           asig9, asig10, asig11, asig12, asig13, asig14, asig15, asig16

csound doc: <https://csound.com/docs/manual/outx.html>
-}
outx :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> SE ()
outx b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5 <*> (lift . unSig) b6 <*> (lift . unSig) b7 <*> (lift . unSig) b8 <*> (lift . unSig) b9 <*> (lift . unSig) b10 <*> (lift . unSig) b11 <*> (lift . unSig) b12 <*> (lift . unSig) b13 <*> (lift . unSig) b14 <*> (lift . unSig) b15 <*> (lift . unSig) b16
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 =
      opcsDep_
        "outx"
        [
          ( Xr
          , [Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar]
          )
        ]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        ]

{- |
Writes multi-channel audio data from a ZAK array to an external device or stream.

>  outz  ksig1

csound doc: <https://csound.com/docs/manual/outz.html>
-}
outz :: Sig -> SE ()
outz b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "outz" [(Xr, [Kr])] [a1]

{- |
Deprecated. Writes audio output to a disk file.

The usage of soundout is discouraged. Please use  fout instead.

>  soundout   asig1, ifilcod [, iformat]

csound doc: <https://csound.com/docs/manual/soundout.html>
-}
soundout :: Sig -> Str -> SE ()
soundout b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unStr) b2
  where
    f a1 a2 = opcsDep_ "soundout" [(Xr, [Ar, Sr, Ir])] [a1, a2]

{- |
Deprecated. Writes audio output to a disk file.

The usage of soundouts is discouraged. Please use  fout instead.

>  soundouts   asigl, asigr, ifilcod [, iformat]

csound doc: <https://csound.com/docs/manual/soundouts.html>
-}
soundouts :: Sig -> Sig -> Str -> SE ()
soundouts b1 b2 b3 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unStr) b3
  where
    f a1 a2 a3 = opcsDep_ "soundouts" [(Xr, [Ar, Ar, Sr, Ir])] [a1, a2, a3]

-- Software Bus.

{- |
Reads data from the software bus

Reads data from a channel of the inward software bus.

> kval  chani  kchan
> aval  chani  kchan

csound doc: <https://csound.com/docs/manual/chani.html>
-}
chani :: Sig -> SE Sig
chani b1 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep "chani" [(Kr, [Kr]), (Ar, [Kr])] [a1]

{- |
Send data to the outwards software bus

Send data to a channel of the outward software bus.

>  chano  kval, kchan
>  chano  aval, kchan

csound doc: <https://csound.com/docs/manual/chano.html>
-}
chano :: Sig -> Sig -> SE ()
chano b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "chano" [(Xr, [Kr, Kr])] [a1, a2]

{- |
Declare a channel of the named software bus.

Declare a channel of the named software bus, with setting optional
      parameters in the case of a control channel. If the channel does not
      exist yet, it is created, with an inital value of zero or empty string.
      Otherwise, the type (control, audio, or string) of the existing channel
      must match the declaration, or an init error occurs. The input/output
      mode of an existing channel is updated so that it becomes the bitwise
      OR of the previous and the newly specified value.

>   chn_k  Sname, imode[, itype, idflt, imin, ima, ix, iy,
>           iwidth, iheight, Sattributes]

csound doc: <https://csound.com/docs/manual/chn.html>
-}
chn_k :: Str -> D -> SE ()
chn_k b1 b2 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "chn_k" [(Xr, [Sr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Sr])] [a1, a2]

{- |
Declare a channel of the named software bus.

Declare a channel of the named software bus, with setting optional
      parameters in the case of a control channel. If the channel does not
      exist yet, it is created, with an inital value of zero or empty string.
      Otherwise, the type (control, audio, or string) of the existing channel
      must match the declaration, or an init error occurs. The input/output
      mode of an existing channel is updated so that it becomes the bitwise
      OR of the previous and the newly specified value.

>   chn_a  Sname, imode

csound doc: <https://csound.com/docs/manual/chn.html>
-}
chn_a :: Str -> D -> SE ()
chn_a b1 b2 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "chn_a" [(Xr, [Sr, Ir])] [a1, a2]

{- |
Declare a channel of the named software bus.

Declare a channel of the named software bus, with setting optional
      parameters in the case of a control channel. If the channel does not
      exist yet, it is created, with an inital value of zero or empty string.
      Otherwise, the type (control, audio, or string) of the existing channel
      must match the declaration, or an init error occurs. The input/output
      mode of an existing channel is updated so that it becomes the bitwise
      OR of the previous and the newly specified value.

>   chn_S  Sname, imode
>   chn_S  Sname, Smode

csound doc: <https://csound.com/docs/manual/chn.html>
-}
chn_S :: Str -> D -> SE ()
chn_S b1 b2 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "chn_S" [(Xr, [Sr, Ir])] [a1, a2]

{- |
Clears an audio output channel of the named software bus.

Clears an audio channel of the named software bus to zero.
      Implies declaring the channel with imode=2 (see also
      chn_a).

>  chnclear  Sname1[, Sname2,...]

csound doc: <https://csound.com/docs/manual/chnclear.html>
-}
chnclear :: Str -> SE ()
chnclear b1 =
  SE $ join $ f <$> (lift . unStr) b1
  where
    f a1 = opcsDep_ "chnclear" [(Xr, (repeat Sr))] [a1]

{- |
Export a global variable as a channel of the bus.

Export a global variable as a channel of the bus; the channel
      should not already exist, otherwise an init error occurs.
      This opcode is normally called from the orchestra header, and allows
      the host application to read or write orchestra variables directly,
      without having to use
      chnget or
      chnset to copy data.

> gival  chnexport  Sname, imode[, itype, idflt, imin, imax]
> gkval  chnexport  Sname, imode[, itype, idflt, imin, imax]
> gaval  chnexport  Sname, imode
> gSval  chnexport  Sname, imode

csound doc: <https://csound.com/docs/manual/chnexport.html>
-}
chnexport :: Str -> D -> Str
chnexport b1 b2 =
  Str $ f <$> unStr b1 <*> unD b2
  where
    f a1 a2 =
      opcs
        "chnexport"
        [ (Ir, [Sr, Ir, Ir, Ir, Ir, Ir])
        , (Kr, [Sr, Ir, Ir, Ir, Ir, Ir])
        , (Ar, [Sr, Ir])
        , (Sr, [Sr, Ir])
        ]
        [a1, a2]

{- |
Reads data from the software bus.

Reads data from a channel of the inward named software bus.
      Implies declaring the channel with imode=1 (see also
      chn_k, chn_a, and chn_S).

> ival  chnget  Sname
> kval  chnget  Sname
> aval  chnget  Sname
> Sval  chnget  Sname

csound doc: <https://csound.com/docs/manual/chnget.html>
-}
chnget :: Str -> SE Str
chnget b1 =
  fmap (Str . return) $ SE $ join $ f <$> (lift . unStr) b1
  where
    f a1 = opcsDep "chnget" [(Ir, [Sr]), (Kr, [Sr]), (Ar, [Sr]), (Sr, [Sr])] [a1]

{- |
Reads data from the software bus.

Reads data from a channel of the inward named software bus.
      Implies declaring the channel with imode=1 (see also
      chn_k, chn_a, and chn_S).

> Sval  chngetks  Sname

csound doc: <https://csound.com/docs/manual/chnget.html>
-}
chngetks :: Str -> Str
chngetks b1 =
  Str $ f <$> unStr b1
  where
    f a1 = opcs "chngetks" [(Sr, [Sr])] [a1]

--
-- > ival[]  chngeti  Sname[]
--
-- csound doc: <https://csound.com/docs/manual/chnget.html>
chngeti :: Str -> D
chngeti b1 =
  D $ f <$> unStr b1
  where
    f a1 = opcs "chngeti" [(Ir, [Sr])] [a1]

--
-- > kval[]  chngetk  Sname[]
--
-- csound doc: <https://csound.com/docs/manual/chnget.html>
chngetk :: Str -> Sig
chngetk b1 =
  Sig $ f <$> unStr b1
  where
    f a1 = opcs "chngetk" [(Kr, [Sr])] [a1]

--
-- > aval[]  chngeta  Sname[]
--
-- csound doc: <https://csound.com/docs/manual/chnget.html>
chngeta :: Str -> Sig
chngeta b1 =
  Sig $ f <$> unStr b1
  where
    f a1 = opcs "chngeta" [(Ar, [Sr])] [a1]

--
-- > Sval[]  chngets  Sname[]
--
-- csound doc: <https://csound.com/docs/manual/chnget.html>
chngets :: Str -> Str
chngets b1 =
  Str $ f <$> unStr b1
  where
    f a1 = opcs "chngets" [(Sr, [Sr])] [a1]

{- |
Writes audio data to the named software bus, mixing to the previous
      output.

Adds an audio signal to a channel of the named software bus.
      Implies declaring the channel with imode=2 (see also
      chn_a).

>  chnmix  aval, Sname

csound doc: <https://csound.com/docs/manual/chnmix.html>
-}
chnmix :: Sig -> Str -> SE ()
chnmix b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unStr) b2
  where
    f a1 a2 = opcsDep_ "chnmix" [(Xr, [Ar, Sr])] [a1, a2]

{- |
Query parameters of a channel.

Query parameters of a channel (if it does not exist, all
      returned values are zero).

> itype, imode, ictltype, idflt, imin, imax  chnparams  Sname

csound doc: <https://csound.com/docs/manual/chnparams.html>
-}
chnparams :: forall a. (Tuple a) => Str -> a
chnparams b1 =
  pureTuple $ f <$> unStr b1
  where
    f a1 = mopcs "chnparams" ([Ir, Ir, Ir, Ir, Ir, Ir], [Sr]) [a1]

{- |
Writes data to the named software bus.

Write to a channel of the named software bus. Implies declaring the
      channel with imod=2 (see also
      chn_k, chn_a, and chn_S).

>  chnset  ival, Sname
>  chnset  kval, Sname
>  chnset  aval, Sname
>  chnset  Sval, Sname

csound doc: <https://csound.com/docs/manual/chnset.html>
-}
chnset :: D -> Str -> SE ()
chnset b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unStr) b2
  where
    f a1 a2 = opcsDep_ "chnset" [(Xr, [Ir, Sr])] [a1, a2]

{- |
Writes data to the named software bus.

Write to a channel of the named software bus. Implies declaring the
      channel with imod=2 (see also
      chn_k, chn_a, and chn_S).

>  chnsetks  Sval, Sname

csound doc: <https://csound.com/docs/manual/chnset.html>
-}
chnsetks :: Str -> Str -> SE ()
chnsetks b1 b2 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unStr) b2
  where
    f a1 a2 = opcsDep_ "chnsetks" [(Xr, [Sr, Sr])] [a1, a2]

--
-- >  chnseti  ival[], []Sname
--
-- csound doc: <https://csound.com/docs/manual/chnset.html>
chnseti :: D -> SE ()
chnseti b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "chnseti" [(Xr, [Ir, Sr])] [a1]

--
-- >  chnsetk  kval[], []Sname
--
-- csound doc: <https://csound.com/docs/manual/chnset.html>
chnsetk :: Sig -> SE ()
chnsetk b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "chnsetk" [(Xr, [Kr, Sr])] [a1]

--
-- >  chnseta  aval[], []Sname
--
-- csound doc: <https://csound.com/docs/manual/chnset.html>
chnseta :: Sig -> SE ()
chnseta b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "chnseta" [(Xr, [Ar, Sr])] [a1]

--
-- >  chnsets  Sval[], []Sname
--
-- csound doc: <https://csound.com/docs/manual/chnset.html>
chnsets :: Str -> SE ()
chnsets b1 =
  SE $ join $ f <$> (lift . unStr) b1
  where
    f a1 = opcsDep_ "chnsets" [(Xr, [Sr, Sr])] [a1]

{- |
Sets the local ksmps value in an instrument or user-defined opcode block

Sets the local ksmps value in an instrument or user-defined opcode block.

>  setksmps  iksmps

csound doc: <https://csound.com/docs/manual/setksmps.html>
-}
setksmps :: D -> SE ()
setksmps b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "setksmps" [(Xr, [Ir])] [a1]

{- |
Passes variables to a user-defined opcode block,

The xin and xout opcodes copy variables to and from the opcode definition, allowing communication with the calling instrument.

> xinarg1 [, xinarg2] ... [xinargN]  xin

csound doc: <https://csound.com/docs/manual/xin.html>
-}
xin :: forall a. (Tuple a) => a
xin =
  pureTuple $ return $ f
  where
    f = mopcs "xin" ((repeat Xr), []) []

{- |
Retrieves variables from a user-defined opcode block,

The xin and xout opcodes copy variables to and from the opcode definition, allowing communication with the calling instrument.

>  xout  xoutarg1 [, xoutarg2] ... [, xoutargN]

csound doc: <https://csound.com/docs/manual/xout.html>
-}
xout :: [Sig] -> SE ()
xout b1 =
  SE $ join $ f <$> mapM (lift . unSig) b1
  where
    f a1 = opcsDep_ "xout" [(Xr, (repeat Xr))] a1

-- Printing and Display.

{- |
Displays the Fourier Transform of an audio or control signal.

These units will print orchestra init-values, or produce graphic display of orchestra control signals and audio signals. Uses X11 windows if enabled, else (or if -g flag is set) displays are approximated in ASCII characters.

>  dispfft  xsig, iprd, iwsiz [, iwtyp] [, idbout] [, iwtflg] [,imin] [,imax]

csound doc: <https://csound.com/docs/manual/dispfft.html>
-}
dispfft :: Sig -> D -> D -> SE ()
dispfft b1 b2 b3 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unD) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "dispfft" [(Xr, [Xr, Ir, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3]

{- |
Displays the audio or control signals as an amplitude vs. time graph.

These units will print orchestra init-values, or produce graphic display of orchestra control signals and audio signals. Uses X11 windows if enabled, else (or if -g flag is set) displays are approximated in ASCII characters.

>  display  xsig, iprd [, inprds] [, iwtflg]

csound doc: <https://csound.com/docs/manual/display.html>
-}
display :: Sig -> D -> SE ()
display b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "display" [(Xr, [Xr, Ir, Ir, Ir])] [a1, a2]

{- |
Allows text to be displayed from instruments like sliders

Allows text to be displayed from instruments like sliders etc. (only on Unix and Windows at present)

>  flashtxt   iwhich, String

csound doc: <https://csound.com/docs/manual/flashtxt.html>
-}
flashtxt :: D -> Str -> SE ()
flashtxt b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unStr) b2
  where
    f a1 a2 = opcsDep_ "flashtxt" [(Xr, [Ir, Sr])] [a1, a2]

{- |
Displays the values init (i-rate) variables.

These units will print orchestra init-values.

>  print  iarg [, iarg1] [, iarg2] [...]

csound doc: <https://csound.com/docs/manual/print.html>
-}
print' :: [D] -> SE ()
print' b1 =
  SE $ join $ f <$> mapM (lift . unD) b1
  where
    f a1 = opcsDep_ "print" [(Xr, (repeat Ir))] a1

{- |
printf-style formatted output

printf and printf_i write
    formatted output, similarly to the C function
    printf(). printf_i runs at i-time only, while
    printf runs both at initialization and
    performance time.

>  printf_i  Sfmt, itrig, [iarg1[, iarg2[, ... ]]]

csound doc: <https://csound.com/docs/manual/printf.html>
-}
printf_i :: Str -> D -> [D] -> SE ()
printf_i b1 b2 b3 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> mapM (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "printf_i" [(Xr, [Sr] ++ (repeat Ir))] ([a1, a2] ++ a3)

{- |
printf-style formatted output

printf and printf_i write
    formatted output, similarly to the C function
    printf(). printf_i runs at i-time only, while
    printf runs both at initialization and
    performance time.

>  printf  Sfmt, ktrig, [xarg1[, xarg2[, ... ]]]

csound doc: <https://csound.com/docs/manual/printf.html>
-}
printf :: Str -> Sig -> [Sig] -> SE ()
printf b1 b2 b3 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unSig) b2 <*> mapM (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep_ "printf" [(Xr, [Sr, Kr] ++ (repeat Xr))] ([a1, a2] ++ a3)

{- |
Prints one k-rate value at specified intervals.

>  printk  itime, kval [, ispace] [, inamed]

csound doc: <https://csound.com/docs/manual/printk.html>
-}
printk :: D -> Sig -> SE ()
printk b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "printk" [(Xr, [Ir, Kr, Ir, Ir])] [a1, a2]

{- |
Prints a new value every time a control variable changes.

>  printk2  kvar [, inumspaces] [, inamed]

csound doc: <https://csound.com/docs/manual/printk2.html>
-}
printk2 :: Sig -> SE ()
printk2 b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "printk2" [(Xr, [Kr, Ir, Ir])] [a1]

{- |
Prints at k-rate using a printf() style syntax.

>  printks  "string", itime [, xval1] [, xval2] [...]

csound doc: <https://csound.com/docs/manual/printks.html>
-}
printks :: Str -> D -> [Sig] -> SE ()
printks b1 b2 b3 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> mapM (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep_ "printks" [(Xr, [Sr, Ir] ++ (repeat Xr))] ([a1, a2] ++ a3)

{- |
Prints a new value every time a control variable changes using a
      printf() style syntax.

>  printks2  "string", kval

csound doc: <https://csound.com/docs/manual/printks2.html>
-}
printks2 :: Str -> Sig -> SE ()
printks2 b1 b2 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "printks2" [(Xr, [Sr, Kr])] [a1, a2]

--
-- >  println  "string", [, xval1] [, xval2] [...]
--
-- csound doc: <https://csound.com/docs/manual/println.html>
println :: Str -> SE ()
println b1 =
  SE $ join $ f <$> (lift . unStr) b1
  where
    f a1 = opcsDep_ "println" [(Xr, [Sr] ++ (repeat Xr))] [a1]

{- |
Prints at init-time using a printf() style syntax.

>  prints  "string" [, xval1] [, xval2] [...]

csound doc: <https://csound.com/docs/manual/prints.html>
-}
prints :: Str -> [Sig] -> SE ()
prints b1 b2 =
  SE $ join $ f <$> (lift . unStr) b1 <*> mapM (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "prints" [(Xr, [Sr] ++ (repeat Xr))] ([a1] ++ a2)

--
-- >  printsk  "string", [, xval1] [, xval2] [...]
--
-- csound doc: <https://csound.com/docs/manual/printsk.html>
printsk :: Str -> SE ()
printsk b1 =
  SE $ join $ f <$> (lift . unStr) b1
  where
    f a1 = opcsDep_ "printsk" [(Xr, [Sr] ++ (repeat Xr))] [a1]

-- Soundfile Queries.

{- |
Returns the number of bits in each sample in a sound file.

> ir  filebit  ifilcod [, iallowraw]

csound doc: <https://csound.com/docs/manual/filebit.html>
-}
filebit :: Str -> D
filebit b1 =
  D $ f <$> unStr b1
  where
    f a1 = opcs "filebit" [(Ir, [Sr, Ir])] [a1]

{- |
Returns the length of a sound file.

> ir  filelen  ifilcod, [iallowraw]

csound doc: <https://csound.com/docs/manual/filelen.html>
-}
filelen :: Str -> D
filelen b1 =
  D $ f <$> unStr b1
  where
    f a1 = opcs "filelen" [(Ir, [Sr, Ir])] [a1]

{- |
Returns the number of channels in a sound file.

> ir  filenchnls  ifilcod [, iallowraw]

csound doc: <https://csound.com/docs/manual/filenchnls.html>
-}
filenchnls :: Str -> D
filenchnls b1 =
  D $ f <$> unStr b1
  where
    f a1 = opcs "filenchnls" [(Ir, [Sr, Ir])] [a1]

{- |
Returns the peak absolute value of a sound file.

> ir  filepeak  ifilcod [, ichnl]

csound doc: <https://csound.com/docs/manual/filepeak.html>
-}
filepeak :: Str -> D
filepeak b1 =
  D $ f <$> unStr b1
  where
    f a1 = opcs "filepeak" [(Ir, [Sr, Ir])] [a1]

{- |
Returns the sample rate of a sound file.

> ir  filesr  ifilcod [, iallowraw]

csound doc: <https://csound.com/docs/manual/filesr.html>
-}
filesr :: Str -> D
filesr b1 =
  D $ f <$> unStr b1
  where
    f a1 = opcs "filesr" [(Ir, [Sr, Ir])] [a1]

{- |
Checks that a file can be used.

Returns 1 if the sound file is valid, or 0 if not.

> ir  filevalid  ifilcod
> kr  filevalid  ifilcod

csound doc: <https://csound.com/docs/manual/filevalid.html>
-}
filevalid :: Str -> Sig
filevalid b1 =
  Sig $ f <$> unStr b1
  where
    f a1 = opcs "filevalid" [(Ir, [Sr]), (Kr, [Sr])] [a1]

{- |
Returns the length of an MP3 sound file.

> ir  mp3len  ifilcod

csound doc: <https://csound.com/docs/manual/mp3len.html>
-}
mp3len :: Str -> D
mp3len b1 =
  D $ f <$> unStr b1
  where
    f a1 = opcs "mp3len" [(Ir, [Sr])] [a1]
