module Csound.Typed.Opcode.ZakPatchSystem (
  zacl,
  zakinit,
  zamod,
  zar,
  zarg,
  zaw,
  zawm,
  zir,
  ziw,
  ziwm,
  zkcl,
  zkmod,
  zkr,
  zkw,
  zkwm,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Csound.Dynamic
import Csound.Typed

--

{- |
Clears one or more variables in the za space.

>  zacl  kfirst [, klast]

csound doc: <https://csound.com/docs/manual/zacl.html>
-}
zacl :: Sig -> SE ()
zacl b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "zacl" [(Xr, [Kr, Kr])] [a1]

{- |
Establishes zak space.

Establishes zak space. Must be called only once.

>  zakinit  isizea, isizek

csound doc: <https://csound.com/docs/manual/zakinit.html>
-}
zakinit :: D -> D -> SE ()
zakinit b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "zakinit" [(Xr, [Ir, Ir])] [a1, a2]

{- |
Modulates one a-rate signal by a second one.

> ares  zamod  asig, kzamod

csound doc: <https://csound.com/docs/manual/zamod.html>
-}
zamod :: Sig -> Sig -> Sig
zamod b1 b2 =
  Sig $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = opcs "zamod" [(Ar, [Ar, Kr])] [a1, a2]

{- |
Reads from a location in za space at a-rate.

> ares  zar  kndx

csound doc: <https://csound.com/docs/manual/zar.html>
-}
zar :: Sig -> Sig
zar b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "zar" [(Ar, [Kr])] [a1]

{- |
Reads from a location in za space at a-rate, adds some gain.

> ares  zarg  kndx, kgain

csound doc: <https://csound.com/docs/manual/zarg.html>
-}
zarg :: Sig -> Sig -> Sig
zarg b1 b2 =
  Sig $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = opcs "zarg" [(Ar, [Kr, Kr])] [a1, a2]

{- |
Writes to a za variable at a-rate without mixing.

>  zaw  asig, kndx

csound doc: <https://csound.com/docs/manual/zaw.html>
-}
zaw :: Sig -> Sig -> SE ()
zaw b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "zaw" [(Xr, [Ar, Kr])] [a1, a2]

{- |
Writes to a za variable at a-rate with mixing.

>  zawm  asig, kndx [, imix]

csound doc: <https://csound.com/docs/manual/zawm.html>
-}
zawm :: Sig -> Sig -> SE ()
zawm b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "zawm" [(Xr, [Ar, Kr, Ir])] [a1, a2]

{- |
Reads from a location in zk space at i-rate.

> ir  zir  indx

csound doc: <https://csound.com/docs/manual/zir.html>
-}
zir :: D -> D
zir b1 =
  D $ f <$> unD b1
  where
    f a1 = opcs "zir" [(Ir, [Ir])] [a1]

{- |
Writes to a zk variable at i-rate without mixing.

>  ziw  isig, indx

csound doc: <https://csound.com/docs/manual/ziw.html>
-}
ziw :: D -> D -> SE ()
ziw b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "ziw" [(Xr, [Ir, Ir])] [a1, a2]

{- |
Writes to a zk variable to an i-rate variable with mixing.

>  ziwm  isig, indx [, imix]

csound doc: <https://csound.com/docs/manual/ziwm.html>
-}
ziwm :: D -> D -> SE ()
ziwm b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "ziwm" [(Xr, [Ir, Ir, Ir])] [a1, a2]

{- |
Clears one or more variables in the zk space.

>  zkcl  kfirst, klast

csound doc: <https://csound.com/docs/manual/zkcl.html>
-}
zkcl :: Sig -> Sig -> SE ()
zkcl b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "zkcl" [(Xr, [Kr, Kr])] [a1, a2]

{- |
Facilitates the modulation of one signal by another.

> kres  zkmod  ksig, kzkmod

csound doc: <https://csound.com/docs/manual/zkmod.html>
-}
zkmod :: Sig -> Sig -> Sig
zkmod b1 b2 =
  Sig $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = opcs "zkmod" [(Kr, [Kr, Kr])] [a1, a2]

{- |
Reads from a location in zk space at k-rate.

> kres  zkr  kndx

csound doc: <https://csound.com/docs/manual/zkr.html>
-}
zkr :: Sig -> Sig
zkr b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "zkr" [(Kr, [Kr])] [a1]

{- |
Writes to a zk variable at k-rate without mixing.

>  zkw  kval, kndx

csound doc: <https://csound.com/docs/manual/zkw.html>
-}
zkw :: Sig -> Sig -> SE ()
zkw b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "zkw" [(Xr, [Kr, Kr])] [a1, a2]

{- |
Writes to a zk variable at k-rate with mixing.

>  zkwm  ksig, kndx [, imix]

csound doc: <https://csound.com/docs/manual/zkwm.html>
-}
zkwm :: Sig -> Sig -> SE ()
zkwm b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "zkwm" [(Xr, [Kr, Kr, Ir])] [a1, a2]
