-- | Data
module Csound.Opcode.Data (

    -----------------------------------------------------
    -- * Buffer and Function tables

    -- ** Creating Function Tables (Buffers)
    gen,

    -- ** Writing To Tables
    tableiw, tablew, tabw_i, tabw, 

    -- ** Reading From Tables
    table, tablei, table3, tab_i, tab, 

    -- ** Saving Tables To Files
    
    -- ** Reading Tables From Files

    -----------------------------------------------------
    -- * Signal Input and Output,  Sample and Loop Playback, Soundfonts

    -- ** Signal Input And Output
    inch, outch,

    -- ** Sample Playback With Optional Looping
    flooper2, sndloop,

    -- ** Soundfonts And Fluid Opcodes

    -----------------------------------------------------
    -- *  File Input and Output

    -- ** Sound File Input
    soundin, diskin2, mp3in,

    -- ** Sound File Queries

    -- ** Sound File Output
    fout,

    -- ** Non-Soundfile Input And Output

    -----------------------------------------------------
    -- * Converters of Data Types

    downsamp, upsamp, interp,

    -----------------------------------------------------
    -- * Printing and Strings

    -- ** Simple Printing
    printi, printk,

    -- ** Formatted Printing

    -- ** String Variables
    sprintf, sprintfk

    -- ** String Manipulation And Conversion

) where

import Data.Fix

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

-----------------------------------------------------
-- * Buffer and Function tables

-- ** Creating Function Tables (Buffers)
gen :: Int -> Int -> [Double] -> Tab
gen = Ftable 

-- ** Writing To Tables

-- |
--
-- tablew asig, andx, ifn [, ixmode] [, ixoff] [, iwgmode]
-- tablew isig, indx, ifn [, ixmode] [, ixoff] [, iwgmode]
-- tablew ksig, kndx, ifn [, ixmode] [, ixoff] [, iwgmode]
--
-- doc: <http://www.csounds.com/manual/html/.html>
tablew :: Sig -> Sig -> Tab -> SE ()
tablew a1 a2 a3 = se_ $ opc3 "tablew" (map sign [a, k, i]) a1 a2 a3
    where sign t = (x, t:t:is 4)

-- |
--
-- tableiw isig, indx, ifn [, ixmode] [, ixoff] [, iwgmode]
--
-- doc: <http://www.csounds.com/manual/html/.html>
tableiw :: D -> D -> Tab -> SE ()
tableiw a1 a2 a3 = se_ $ opc3 "tableiw" [(i, is 6)] a1 a2 a3

-- |
--
-- tabw ksig, kndx, ifn [,ixmode]
-- tabw asig, andx, ifn [,ixmode]
--
-- doc: <http://www.csounds.com/manual/html/.html>
tabw :: Sig -> Sig -> Tab -> SE ()
tabw a1 a2 a3 = se_ $ opc3 "tabw" (map sign [a, k]) a1 a2 a3
    where sign t = (x, t:t:is 2)

-- |
--
-- tabw_i isig, indx, ifn [,ixmode]
--
-- doc: <http://www.csounds.com/manual/html/.html>
tabw_i :: D -> D -> Tab -> SE ()
tabw_i a1 a2 a3 = se_ $ opc3 "tabw_i" [(x, is 4)] a1 a2 a3

-- ** Reading From Tables

-- |
--
-- ares table andx, ifn [, ixmode] [, ixoff] [, iwrap]
-- ires table indx, ifn [, ixmode] [, ixoff] [, iwrap]
-- kres table kndx, ifn [, ixmode] [, ixoff] [, iwrap]
--
-- doc: <http://www.csounds.com/manual/html/.html>
table, tablei :: Sig -> Tab -> Sig

table = mkTable "table"
tablei = mkTable "tablei"
table3 = mkTable "table3"

mkTable :: Name -> Sig -> Tab -> Sig
mkTable name = opc2 name [
    (a, a:rest),
    (k, k:rest),
    (i, i:rest)]
    where rest = [i, i, i]

-- |
--
-- kr tab kndx, ifn[, ixmode]
-- ar tab xndx, ifn[, ixmode]
--
-- doc: <http://www.csounds.com/manual/html/.html>
tab :: Sig -> Tab -> Sig
tab = opc2 "tab" [
    (a, [x,i,i]),
    (k, [k,i,i])]

-- |
--
-- ir tab_i indx, ifn[, ixmode]
--
-- doc: <http://www.csounds.com/manual/html/.html>
tab_i :: D -> Tab -> D
tab_i = opc2 "tab_i" [(i, [i,i,i])]

-- ** Saving Tables To Files

{-
-- ftsave "filename", iflag, ifn1 [, ifn2] [...]
ftsave :: S -> I -> [Tab] -> SE ()
ftsave a1 a2 a3 = opcs "ftsave" [(x, repeat i)] (phi a1 : phi a2 : map phi a3)
    where phi :: Val a => a -> E
          phi = Fix . unwrap  

-- ftsavek "filename", ktrig, iflag, ifn1 [, ifn2] [...]
ftsavek :: S -> Sig -> I -> [Tab] -> SE ()
ftsavek a1 a2 a3 a4 = opcs "ftsavek" [(x, repeat i)] (phi a1 : phi a2 : phi a3 : map phi a4)
    where phi :: Val a => a -> E
          phi = Fix . unwrap  
-}

-- ** Reading Tables From Files

-----------------------------------------------------
-- * Signal Input and Output,  Sample and Loop Playback, Soundfonts

-- ** Signal Input And Output

-- |
--
-- ain1[, ...] inch kchan1[,...]
--
-- doc: <http://www.csounds.com/manual/html/.html>
inch :: MultiOut a => [Sig] -> a
inch = mopcs "inch" (repeat a, repeat k)

-- |
--
--
-- doc: <http://www.csounds.com/manual/html/.html>
outch :: [(Sig, Sig)] -> SE ()
outch ts = se_ $ opcs "outch" [(x, cycle [a,k])] $ (\(a, b) -> [a, b]) =<< ts

-- ** Sample Playback With Optional Looping

-- |
--
-- asig flooper2 kamp, kpitch, kloopstart, kloopend, kcrossfade, ifn \
--       [, istart, imode, ifenv, iskip]
--
-- doc: <http://www.csounds.com/manual/html/.html>
flooper2 :: Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Sig  
flooper2 = opc6 "flooper2" [(a, ks 5 ++ is 5)]

-- |
--
-- asig, krec sndloop ain, kpitch, ktrig, idur, ifad
--
-- doc: <http://www.csounds.com/manual/html/.html>
sndloop :: Sig -> Sig -> Sig -> D -> D -> (Sig, Sig)
sndloop = mopc5 "sndloop" ([a, k], [a,k,k,i,i])

-- ** Soundfonts And Fluid Opcodes

-----------------------------------------------------
-- *  File Input and Output

-- ** Sound File Input

-- |
--
-- ar1[, ar2[, ar3[, ... a24]]] soundin ifilcod [, iskptim] [, iformat] \
--      [, iskipinit] [, ibufsize]
--
-- doc: <http://www.csounds.com/manual/html/.html>
soundin :: MultiOut a => S -> a
soundin = mopc1 "soundin" (repeat a, s:is 4)

-- |
--
-- a1[, a2[, ... aN]] diskin2 ifilcod, kpitch[, iskiptim \
--       [, iwrap[, iformat [, iwsize[, ibufsize[, iskipinit]]]]]]
--
-- doc: <http://www.csounds.com/manual/html/.html>
diskin2 :: MultiOut a => S -> Sig -> a
diskin2 = mopc2 "diskin2" (repeat a, s:k:is 6)

-- |
--
-- ar1, ar2 mp3in ifilcod[, iskptim, iformat, iskipinit, ibufsize]
--
-- doc: <http://www.csounds.com/manual/html/.html>
mp3in :: S -> (Sig, Sig)
mp3in = mopc1 "mp3in" ([a,a], s:is 4)

-- ** Sound File Queries

-- ** Sound File Output

-- |
--
-- fout ifilename, iformat, aout1 [, aout2, aout3,...,aoutN]
--
-- doc: <http://www.csounds.com/manual/html/.html>
fout :: [Sig] -> SE ()
fout as = se_ $ opcs "fout" [(x, repeat a)] as

-- ** Non-Soundfile Input And Output

-----------------------------------------------------
-- * Converters of Data Types

-- |
--
-- kres downsamp asig [, iwlen]
--
-- doc: <http://www.csounds.com/manual/html/.html>
downsamp :: Sig -> Sig 
downsamp = opc1 "downsamp" [(k, [a,i])]

-- |
--
-- ares upsamp ksig
--
-- doc: <http://www.csounds.com/manual/html/.html>
upsamp :: Sig -> Sig
upsamp = opc1 "upsamp" [(a, [k])]

-- |
--
-- ares interp ksig [, iskip] [, imode]
--
-- doc: <http://www.csounds.com/manual/html/.html>
interp :: Sig -> Sig
interp = opc1 "interp" [(a, [k,i,i])]

-----------------------------------------------------
-- * Printing and Strings

-- ** Simple Printing

-- |
--
-- print iarg [, iarg1] [, iarg2] [...]
--
-- doc: <http://www.csounds.com/manual/html/.html>
printi :: [D] -> SE ()
printi a1 = se_ $ opcs "print" [(x, repeat i)] a1

-- |
--
-- printk itime, kval [, ispace]
--
-- doc: <http://www.csounds.com/manual/html/.html>
printk :: D -> Sig -> SE ()
printk a1 a2 = se_ $ opc2 "printk" [(x, [i,k,i])] a1 a2


-- ** Formatted Printing

-- ** String Variables

-- |
--
-- Sdst sprintf Sfmt, xarg1[, xarg2[, ... ]]
--
-- doc: <http://www.csounds.com/manual/html/.html>
sprintf :: S -> [D] -> S
sprintf a1 a2 = opcs "sprintf" [(s, s:repeat i)] (phi a1 : map phi a2)
    where phi :: Val a => a -> E
          phi = Fix . unwrap

-- |
--
-- Sdst sprintfk Sfmt, xarg1[, xarg2[, ... ]]
--
-- doc: <http://www.csounds.com/manual/html/.html>
sprintfk :: S -> [Sig] -> S
sprintfk a1 a2 = opcs "sprintfk" [(s, s:repeat k)] (phi a1 : map phi a2)
    where phi :: Val a => a -> E
          phi = Fix . unwrap

-- ** String Manipulation And Conversion



