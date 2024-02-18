module Csound.Typed.Opcode.Vectorial (
    
    
    -- * Tables.
    vtaba, vtabi, vtabk, vtable1k, vtablea, vtablei, vtablek, vtablewa, vtablewi, vtablewk, vtabwa, vtabwi, vtabwk,
    
    -- * Scalar operations.
    vadd, vadd_i, vexp, vexp_i, vmult, vmult_i, vpow, vpow_i,
    
    -- * Vectorial operations.
    vaddv, vaddv_i, vcopy, vcopy_i, vdivv, vdivv_i, vexpv, vexpv_i, vmap, vmultv, vmultv_i, vpowv, vpowv_i, vsubv, vsubv_i,
    
    -- * Envelopes.
    vexpseg, vlinseg,
    
    -- * Limiting and Wrapping.
    vlimit, vmirror, vwrap,
    
    -- * Delay Paths.
    vdelayk, vecdelay, vport,
    
    -- * Random.
    vrandh, vrandi,
    
    -- * Cellular Automata.
    cell, vcella) where

import Control.Monad.Trans.Class
import Csound.Dynamic
import Csound.Typed

-- Tables.

-- | 
-- Read vectors (from tables -or arrays of vectors).
--
-- This opcode reads vectors from tables at a-rate.
--
-- >  vtaba   andx, ifn, aout1 [, aout2, aout3, .... , aoutN ]
--
-- csound doc: <http://csound.com/docs/manual/vtaba.html>
vtaba ::  Sig -> Tab -> Sig -> SE ()
vtaba b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unTab b2 <*> unSig b3
    where f a1 a2 a3 = opcs "vtaba" [(Xr,[Ar,Ir] ++ (repeat Ar))] [a1,a2,a3]

-- | 
-- Read vectors (from tables -or arrays of vectors).
--
-- This opcode reads vectors from tables.
--
-- >  vtabi   indx, ifn, iout1 [, iout2, iout3, .... , ioutN ]
--
-- csound doc: <http://csound.com/docs/manual/vtabi.html>
vtabi ::  D -> Tab -> D -> SE ()
vtabi b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unD b1 <*> unTab b2 <*> unD b3
    where f a1 a2 a3 = opcs "vtabi" [(Xr,(repeat Ir))] [a1,a2,a3]

-- | 
-- Read vectors (from tables -or arrays of vectors).
--
-- This opcode reads vectors from tables at k-rate.
--
-- >  vtabk   kndx, ifn, kout1 [, kout2, kout3, .... , koutN ]
--
-- csound doc: <http://csound.com/docs/manual/vtabk.html>
vtabk ::  Sig -> Tab -> Sig -> SE ()
vtabk b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unTab b2 <*> unSig b3
    where f a1 a2 a3 = opcs "vtabk" [(Xr,[Kr,Ir] ++ (repeat Kr))] [a1,a2,a3]

-- | 
-- Read a vector (several scalars simultaneously) from a table.
--
-- This opcode reads vectors from tables at k-rate.
--
-- >  vtable1k   kfn,kout1 [, kout2, kout3, .... , koutN ]
--
-- csound doc: <http://csound.com/docs/manual/vtable1k.html>
vtable1k ::  Tab -> Sig -> SE ()
vtable1k b1 b2 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unSig b2
    where f a1 a2 = opcs "vtable1k" [(Xr,(repeat Kr))] [a1,a2]

-- | 
-- Read vectors (from tables -or arrays of vectors).
--
-- This opcode reads vectors from tables at a-rate.
--
-- >  vtablea   andx, kfn, kinterp, ixmode, aout1 [, aout2, aout3, .... , aoutN ]
--
-- csound doc: <http://csound.com/docs/manual/vtablea.html>
vtablea ::  Sig -> Tab -> Sig -> D -> Sig -> SE ()
vtablea b1 b2 b3 b4 b5 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unTab b2 <*> unSig b3 <*> unD b4 <*> unSig b5
    where f a1 a2 a3 a4 a5 = opcs "vtablea" [(Xr,[Ar,Kr,Kr,Ir] ++ (repeat Ar))] [a1,a2,a3,a4,a5]

-- | 
-- Read vectors (from tables -or arrays of vectors).
--
-- This opcode reads vectors from tables.
--
-- >  vtablei   indx, ifn, interp, ixmode, iout1 [, iout2, iout3, .... , ioutN ]
--
-- csound doc: <http://csound.com/docs/manual/vtablei.html>
vtablei ::  D -> Tab -> D -> D -> D -> SE ()
vtablei b1 b2 b3 b4 b5 = SE $ (depT_ =<<) $ lift $ f <$> unD b1 <*> unTab b2 <*> unD b3 <*> unD b4 <*> unD b5
    where f a1 a2 a3 a4 a5 = opcs "vtablei" [(Xr,(repeat Ir))] [a1,a2,a3,a4,a5]

-- | 
-- Read vectors (from tables -or arrays of vectors).
--
-- This opcode reads vectors from tables at k-rate.
--
-- >  vtablek   kndx, kfn, kinterp, ixmode, kout1 [, kout2, kout3, .... , koutN ]
--
-- csound doc: <http://csound.com/docs/manual/vtablek.html>
vtablek ::  Sig -> Tab -> Sig -> D -> Sig -> SE ()
vtablek b1 b2 b3 b4 b5 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unTab b2 <*> unSig b3 <*> unD b4 <*> unSig b5
    where f a1 a2 a3 a4 a5 = opcs "vtablek" [(Xr,[Kr,Kr,Kr,Ir] ++ (repeat Kr))] [a1,a2,a3,a4,a5]

-- | 
-- Write vectors (to tables -or arrays of vectors).
--
-- This opcode writes vectors to tables at a-rate.
--
-- >  vtablewa   andx, kfn, ixmode, ainarg1 [, ainarg2, ainarg3 , .... , ainargN ]
--
-- csound doc: <http://csound.com/docs/manual/vtablewa.html>
vtablewa ::  Sig -> Tab -> D -> Sig -> SE ()
vtablewa b1 b2 b3 b4 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unTab b2 <*> unD b3 <*> unSig b4
    where f a1 a2 a3 a4 = opcs "vtablewa" [(Xr,[Ar,Kr,Ir] ++ (repeat Ar))] [a1,a2,a3,a4]

-- | 
-- Write vectors (to tables -or arrays of vectors).
--
-- This opcode writes vectors to tables at init time.
--
-- >  vtablewi   indx, ifn, ixmode, inarg1 [, inarg2, inarg3 , .... , inargN ]
--
-- csound doc: <http://csound.com/docs/manual/vtablewi.html>
vtablewi ::  D -> Tab -> D -> D -> SE ()
vtablewi b1 b2 b3 b4 = SE $ (depT_ =<<) $ lift $ f <$> unD b1 <*> unTab b2 <*> unD b3 <*> unD b4
    where f a1 a2 a3 a4 = opcs "vtablewi" [(Xr,(repeat Ir))] [a1,a2,a3,a4]

-- | 
-- Write vectors (to tables -or arrays of vectors).
--
-- This opcode writes vectors to tables at k-rate.
--
-- >  vtablewk   kndx, kfn, ixmode, kinarg1 [, kinarg2, kinarg3 , .... , kinargN ]
--
-- csound doc: <http://csound.com/docs/manual/vtablewk.html>
vtablewk ::  Sig -> Tab -> D -> Sig -> SE ()
vtablewk b1 b2 b3 b4 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unTab b2 <*> unD b3 <*> unSig b4
    where f a1 a2 a3 a4 = opcs "vtablewk" [(Xr,[Kr,Kr,Ir] ++ (repeat Kr))] [a1,a2,a3,a4]

-- | 
-- Write vectors (to tables -or arrays of vectors).
--
-- This opcode writes vectors to tables at a-rate.
--
-- >  vtabwa   andx, ifn, ainarg1 [, ainarg2, ainarg3 , .... , ainargN ]
--
-- csound doc: <http://csound.com/docs/manual/vtabwa.html>
vtabwa ::  Sig -> Tab -> Sig -> SE ()
vtabwa b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unTab b2 <*> unSig b3
    where f a1 a2 a3 = opcs "vtabwa" [(Xr,[Ar,Ir] ++ (repeat Ar))] [a1,a2,a3]

-- | 
-- Write vectors (to tables -or arrays of vectors).
--
-- This opcode writes vectors to tables at init time.
--
-- >  vtabwi   indx, ifn, inarg1 [, inarg2, inarg3 , .... , inargN ]
--
-- csound doc: <http://csound.com/docs/manual/vtabwi.html>
vtabwi ::  D -> Tab -> D -> SE ()
vtabwi b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unD b1 <*> unTab b2 <*> unD b3
    where f a1 a2 a3 = opcs "vtabwi" [(Xr,(repeat Ir))] [a1,a2,a3]

-- | 
-- Write vectors (to tables -or arrays of vectors).
--
-- This opcode writes vectors to tables at k-rate.
--
-- >  vtabwk   kndx, ifn, kinarg1 [, kinarg2, kinarg3 , .... , kinargN ]
--
-- csound doc: <http://csound.com/docs/manual/vtabwk.html>
vtabwk ::  Sig -> Tab -> Sig -> SE ()
vtabwk b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unTab b2 <*> unSig b3
    where f a1 a2 a3 = opcs "vtabwk" [(Xr,[Kr,Ir] ++ (repeat Kr))] [a1,a2,a3]

-- Scalar operations.

-- | 
-- Adds a scalar value to a vector in a table.
--
-- >  vadd   ifn, kval, kelements [, kdstoffset] [, kverbose]
--
-- csound doc: <http://csound.com/docs/manual/vadd.html>
vadd ::  Tab -> Sig -> Sig -> SE ()
vadd b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "vadd" [(Xr,[Ir,Kr,Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Adds a scalar value to a vector in a table.
--
-- >  vadd_i   ifn, ival, ielements [, idstoffset]
--
-- csound doc: <http://csound.com/docs/manual/vadd_i.html>
vadd_i ::  Tab -> D -> D -> SE ()
vadd_i b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unD b2 <*> unD b3
    where f a1 a2 a3 = opcs "vadd_i" [(Xr,[Ir,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Performs power-of operations between a vector and a scalar
--
-- >  vexp   ifn, kval, kelements [, kdstoffset] [, kverbose]
--
-- csound doc: <http://csound.com/docs/manual/vexp.html>
vexp ::  Tab -> Sig -> Sig -> SE ()
vexp b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "vexp" [(Xr,[Ir,Kr,Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Performs power-of operations between a vector and a scalar
--
-- >  vexp_i   ifn, ival, ielements[, idstoffset]
--
-- csound doc: <http://csound.com/docs/manual/vexp_i.html>
vexp_i ::  Tab -> D -> D -> SE ()
vexp_i b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unD b2 <*> unD b3
    where f a1 a2 a3 = opcs "vexp_i" [(Xr,[Ir,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Multiplies a vector in a table by a scalar value.
--
-- >  vmult   ifn, kval, kelements [, kdstoffset] [, kverbose]
--
-- csound doc: <http://csound.com/docs/manual/vmult.html>
vmult ::  Tab -> Sig -> Sig -> SE ()
vmult b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "vmult" [(Xr,[Ir,Kr,Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Multiplies a vector in a table by a scalar value.
--
-- >  vmult_i   ifn, ival, ielements [, idstoffset]
--
-- csound doc: <http://csound.com/docs/manual/vmult_i.html>
vmult_i ::  Tab -> D -> D -> SE ()
vmult_i b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unD b2 <*> unD b3
    where f a1 a2 a3 = opcs "vmult_i" [(Xr,[Ir,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Raises each element of a vector to a scalar power.
--
-- >  vpow   ifn, kval, kelements [, kdstoffset] [, kverbose]
--
-- csound doc: <http://csound.com/docs/manual/vpow.html>
vpow ::  Tab -> Sig -> Sig -> SE ()
vpow b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "vpow" [(Xr,[Ir,Kr,Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Raises each element of a vector to a scalar power
--
-- >  vpow_i   ifn, ival, ielements [, idstoffset]
--
-- csound doc: <http://csound.com/docs/manual/vpow_i.html>
vpow_i ::  Tab -> D -> D -> SE ()
vpow_i b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unD b2 <*> unD b3
    where f a1 a2 a3 = opcs "vpow_i" [(Xr,[Ir,Ir,Ir,Ir])] [a1,a2,a3]

-- Vectorial operations.

-- | 
-- Performs addition between two vectorial control signals.
--
-- >  vaddv   ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]
--
-- csound doc: <http://csound.com/docs/manual/vaddv.html>
vaddv ::  Tab -> Tab -> Sig -> SE ()
vaddv b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unTab b2 <*> unSig b3
    where f a1 a2 a3 = opcs "vaddv" [(Xr,[Ir,Ir,Kr,Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Performs addition between two vectorial control signals at init time.
--
-- >  vaddv_i   ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]
--
-- csound doc: <http://csound.com/docs/manual/vaddv_i.html>
vaddv_i ::  Tab -> Tab -> D -> SE ()
vaddv_i b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unTab b2 <*> unD b3
    where f a1 a2 a3 = opcs "vaddv_i" [(Xr,[Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Copies between two vectorial control signals
--
-- >  vcopy   ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [, kverbose]
--
-- csound doc: <http://csound.com/docs/manual/vcopy.html>
vcopy ::  Tab -> Tab -> Sig -> SE ()
vcopy b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unTab b2 <*> unSig b3
    where f a1 a2 a3 = opcs "vcopy" [(Xr,[Ir,Ir,Kr,Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Copies a vector from one table to another.
--
-- >  vcopy_i   ifn1, ifn2, ielements [,idstoffset, isrcoffset]
--
-- csound doc: <http://csound.com/docs/manual/vcopy_i.html>
vcopy_i ::  Tab -> Tab -> D -> SE ()
vcopy_i b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unTab b2 <*> unD b3
    where f a1 a2 a3 = opcs "vcopy_i" [(Xr,[Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Performs division between two vectorial control signals
--
-- >  vdivv   ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]
--
-- csound doc: <http://csound.com/docs/manual/vdivv.html>
vdivv ::  Tab -> Tab -> Sig -> SE ()
vdivv b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unTab b2 <*> unSig b3
    where f a1 a2 a3 = opcs "vdivv" [(Xr,[Ir,Ir,Kr,Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Performs division between two vectorial control signals at init time.
--
-- >  vdivv_i   ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]
--
-- csound doc: <http://csound.com/docs/manual/vdivv_i.html>
vdivv_i ::  Tab -> Tab -> D -> SE ()
vdivv_i b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unTab b2 <*> unD b3
    where f a1 a2 a3 = opcs "vdivv_i" [(Xr,[Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Performs exponential operations between two vectorial control signals
--
-- >  vexpv   ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]
--
-- csound doc: <http://csound.com/docs/manual/vexpv.html>
vexpv ::  Tab -> Tab -> Sig -> SE ()
vexpv b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unTab b2 <*> unSig b3
    where f a1 a2 a3 = opcs "vexpv" [(Xr,[Ir,Ir,Kr,Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Performs exponential operations between two vectorial control signals at init time.
--
-- >  vexpv_i   ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]
--
-- csound doc: <http://csound.com/docs/manual/vexpv_i.html>
vexpv_i ::  Tab -> Tab -> D -> SE ()
vexpv_i b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unTab b2 <*> unD b3
    where f a1 a2 a3 = opcs "vexpv_i" [(Xr,[Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Maps elements from a vector according to indexes contained in another vector.
--
-- Maps elements from a vector onto another according to the indexes of a this vector.
--
-- >  vmap   ifn1, ifn2, ielements [,idstoffset, isrcoffset]
--
-- csound doc: <http://csound.com/docs/manual/vmap.html>
vmap ::  Tab -> Tab -> D -> SE ()
vmap b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unTab b2 <*> unD b3
    where f a1 a2 a3 = opcs "vmap" [(Xr,[Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Performs mutiplication between two vectorial control signals
--
-- >  vmultv   ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]
--
-- csound doc: <http://csound.com/docs/manual/vmultv.html>
vmultv ::  Tab -> Tab -> Sig -> SE ()
vmultv b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unTab b2 <*> unSig b3
    where f a1 a2 a3 = opcs "vmultv" [(Xr,[Ir,Ir,Kr,Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Performs mutiplication between two vectorial control signals at init time.
--
-- >  vmultv_i   ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]
--
-- csound doc: <http://csound.com/docs/manual/vmultv_i.html>
vmultv_i ::  Tab -> Tab -> D -> SE ()
vmultv_i b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unTab b2 <*> unD b3
    where f a1 a2 a3 = opcs "vmultv_i" [(Xr,[Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Performs power-of operations between two vectorial control signals
--
-- >  vpowv  ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]
--
-- csound doc: <http://csound.com/docs/manual/vpowv.html>
vpowv ::  Tab -> Tab -> Sig -> SE ()
vpowv b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unTab b2 <*> unSig b3
    where f a1 a2 a3 = opcs "vpowv" [(Xr,[Ir,Ir,Kr,Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Performs power-of operations between two vectorial control signals at init time.
--
-- >  vpowv_i  ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]
--
-- csound doc: <http://csound.com/docs/manual/vpowv_i.html>
vpowv_i ::  Tab -> Tab -> D -> SE ()
vpowv_i b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unTab b2 <*> unD b3
    where f a1 a2 a3 = opcs "vpowv_i" [(Xr,[Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Performs subtraction between two vectorial control signals
--
-- >  vsubv   ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]
--
-- csound doc: <http://csound.com/docs/manual/vsubv.html>
vsubv ::  Tab -> Tab -> Sig -> SE ()
vsubv b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unTab b2 <*> unSig b3
    where f a1 a2 a3 = opcs "vsubv" [(Xr,[Ir,Ir,Kr,Kr,Kr,Kr])] [a1,a2,a3]

-- | 
-- Performs subtraction between two vectorial control signals at init time.
--
-- >  vsubv_i   ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]
--
-- csound doc: <http://csound.com/docs/manual/vsubv_i.html>
vsubv_i ::  Tab -> Tab -> D -> SE ()
vsubv_i b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unTab b2 <*> unD b3
    where f a1 a2 a3 = opcs "vsubv_i" [(Xr,[Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3]

-- Envelopes.

-- | 
-- Vectorial envelope generator
--
-- Generate exponential vectorial segments
--
-- >  vexpseg   ifnout, ielements, ifn1, idur1, ifn2 [, idur2, ifn3 [...]]
--
-- csound doc: <http://csound.com/docs/manual/vexpseg.html>
vexpseg ::  Tab -> D -> Tab -> D -> Tab -> SE ()
vexpseg b1 b2 b3 b4 b5 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unD b2 <*> unTab b3 <*> unD b4 <*> unTab b5
    where f a1 a2 a3 a4 a5 = opcs "vexpseg" [(Xr,(repeat Ir))] [a1,a2,a3,a4,a5]

-- | 
-- Vectorial envelope generator
--
-- Generate linear vectorial segments
--
-- >  vlinseg   ifnout, ielements, ifn1, idur1, ifn2 [, idur2, ifn3 [...]]
--
-- csound doc: <http://csound.com/docs/manual/vlinseg.html>
vlinseg ::  Tab -> D -> Tab -> D -> Tab -> SE ()
vlinseg b1 b2 b3 b4 b5 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unD b2 <*> unTab b3 <*> unD b4 <*> unTab b5
    where f a1 a2 a3 a4 a5 = opcs "vlinseg" [(Xr,(repeat Ir))] [a1,a2,a3,a4,a5]

-- Limiting and Wrapping.

-- | 
-- Limiting and Wrapping Vectorial Signals
--
-- Limits elements of vectorial control signals.
--
-- >  vlimit   ifn, kmin, kmax, ielements
--
-- csound doc: <http://csound.com/docs/manual/vlimit.html>
vlimit ::  Tab -> Sig -> Sig -> D -> SE ()
vlimit b1 b2 b3 b4 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unSig b2 <*> unSig b3 <*> unD b4
    where f a1 a2 a3 a4 = opcs "vlimit" [(Xr,[Ir,Kr,Kr,Ir])] [a1,a2,a3,a4]

-- | 
-- Limiting and Wrapping Vectorial Signals
--
-- 'Reflects' elements of vectorial control signals on thresholds.
--
-- >  vmirror   ifn, kmin, kmax, ielements
--
-- csound doc: <http://csound.com/docs/manual/vmirror.html>
vmirror ::  Tab -> Sig -> Sig -> D -> SE ()
vmirror b1 b2 b3 b4 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unSig b2 <*> unSig b3 <*> unD b4
    where f a1 a2 a3 a4 = opcs "vmirror" [(Xr,[Ir,Kr,Kr,Ir])] [a1,a2,a3,a4]

-- | 
-- Limiting and Wrapping Vectorial Signals
--
-- Wraps elements of vectorial control signals.
--
-- >  vwrap   ifn, kmin, kmax, ielements
--
-- csound doc: <http://csound.com/docs/manual/vwrap.html>
vwrap ::  Tab -> Sig -> Sig -> D -> SE ()
vwrap b1 b2 b3 b4 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unSig b2 <*> unSig b3 <*> unD b4
    where f a1 a2 a3 a4 = opcs "vwrap" [(Xr,[Ir,Kr,Kr,Ir])] [a1,a2,a3,a4]

-- Delay Paths.

-- | 
-- k-rate variable time delay.
--
-- Variable delay applied to a k-rate signal
--
-- > kout  vdelayk   ksig, kdel, imaxdel [, iskip, imode]
--
-- csound doc: <http://csound.com/docs/manual/vdelayk.html>
vdelayk ::  Sig -> Sig -> D -> Sig
vdelayk b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3
    where f a1 a2 a3 = opcs "vdelayk" [(Kr,[Kr,Kr,Ir,Ir,Ir])] [a1,a2,a3]

-- | 
-- Vectorial Control-rate Delay Paths
--
-- Generate a sort of 'vectorial' delay
--
-- >  vecdelay   ifn, ifnIn, ifnDel, ielements, imaxdel [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/vecdelay.html>
vecdelay ::  Tab -> Tab -> Tab -> D -> D -> SE ()
vecdelay b1 b2 b3 b4 b5 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unTab b2 <*> unTab b3 <*> unD b4 <*> unD b5
    where f a1 a2 a3 a4 a5 = opcs "vecdelay" [(Xr,[Ir,Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3,a4,a5]

-- | 
-- Vectorial Control-rate Delay Paths
--
-- Generate a sort of 'vectorial' portamento
--
-- >  vport  ifn, khtime, ielements [, ifnInit]
--
-- csound doc: <http://csound.com/docs/manual/vport.html>
vport ::  Tab -> Sig -> D -> SE ()
vport b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unSig b2 <*> unD b3
    where f a1 a2 a3 = opcs "vport" [(Xr,[Ir,Kr,Ir,Ir])] [a1,a2,a3]

-- Random.

-- | 
-- Generates a vector of random numbers stored into a table, holding the values for a period of time.
--
-- Generates a vector of random numbers stored into a table, holding the values for a period of time. Generates a sort of 'vectorial band-limited noise'.
--
-- >  vrandh   ifn,  krange, kcps, ielements [, idstoffset] [, iseed] \
-- >           [, isize] [, ioffset]
--
-- csound doc: <http://csound.com/docs/manual/vrandh.html>
vrandh ::  Tab -> Sig -> Sig -> D -> SE ()
vrandh b1 b2 b3 b4 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unSig b2 <*> unSig b3 <*> unD b4
    where f a1 a2 a3 a4 = opcs "vrandh" [(Xr,[Ir,Kr,Kr,Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3,a4]

-- | 
-- Generate a sort of 'vectorial band-limited noise'
--
-- >  vrandi   ifn,  krange, kcps, ielements [, idstoffset] [, iseed] \
-- >           [, isize] [, ioffset]
--
-- csound doc: <http://csound.com/docs/manual/vrandi.html>
vrandi ::  Tab -> Sig -> Sig -> D -> SE ()
vrandi b1 b2 b3 b4 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unSig b2 <*> unSig b3 <*> unD b4
    where f a1 a2 a3 a4 = opcs "vrandi" [(Xr,[Ir,Kr,Kr,Ir,Ir,Ir,Ir,Ir])] [a1,a2,a3,a4]

-- Cellular Automata.

-- | 
-- Cellular Automaton
--
-- One-Dimensional Cellular Automaton. This opcode is the
--          modified version of vcella by Gabriel Maldonado.
--
-- >  cell  ktrig, kreinit, ioutFunc, initStateFunc, iRuleFunc, ielements
--
-- csound doc: <http://csound.com/docs/manual/cell.html>
cell ::  Sig -> Sig -> D -> D -> D -> D -> SE ()
cell b1 b2 b3 b4 b5 b6 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unD b6
    where f a1 a2 a3 a4 a5 a6 = opcs "cell" [(Xr,[Kr,Kr,Ir,Ir,Ir,Ir])] [a1,a2,a3,a4,a5,a6]

-- | 
-- Cellular Automata
--
-- Unidimensional Cellular Automata applied to Csound vectors
--
-- >  vcella  ktrig, kreinit, ioutFunc, initStateFunc, \
-- >           iRuleFunc, ielements, irulelen [, iradius]
--
-- csound doc: <http://csound.com/docs/manual/vcella.html>
vcella ::  Sig -> Sig -> D -> D -> D -> D -> D -> SE ()
vcella b1 b2 b3 b4 b5 b6 b7 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unD b6 <*> unD b7
    where f a1 a2 a3 a4 a5 a6 a7 = opcs "vcella" [(Xr,[Kr,Kr,Ir,Ir,Ir,Ir,Ir,Ir])] [a1
                                                                                  ,a2
                                                                                  ,a3
                                                                                  ,a4
                                                                                  ,a5
                                                                                  ,a6
                                                                                  ,a7]