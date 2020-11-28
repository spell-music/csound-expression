module Csound.Typed.Opcode.MathematicalOperations (
    
    
    -- * Comparators and Accumulators.
    clear, vincr,
    
    -- * Amplitude Functions.
    ampdb, ampdbfs, dbamp, dbfsamp,
    
    -- * Random Functions.
    birnd, rnd,
    
    -- * Opcode Equivalents of Functions.
    divz, mac, maca, polynomial, pow, product', sum', taninv2) where

import Control.Applicative
import Control.Monad.Trans.Class
import Csound.Dynamic
import Csound.Typed

-- Comparators and Accumulators.

-- | 
-- Zeroes a list of audio signals.
--
-- clear zeroes a list of audio signals.
--
-- >  clear  avar1 [, avar2] [, avar3] [...]
--
-- csound doc: <http://csound.com/docs/manual/clear.html>
clear ::  [Sig] -> SE ()
clear b1 = SE $ (depT_ =<<) $ lift $ f <$> mapM unSig b1
    where f a1 = opcs "clear" [(Xr,(repeat Ar))] a1

-- | 
-- Accumulates audio signals.
--
-- vincr increments one audio variable with another signal, i.e. it accumulates output.
--
-- >  vincr  accum, aincr
--
-- csound doc: <http://csound.com/docs/manual/vincr.html>
vincr ::  Sig -> Sig -> SE ()
vincr b1 b2 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "vincr" [(Xr,[Ar,Ar])] [a1,a2]

-- Amplitude Functions.

-- | 
-- Returns the amplitude equivalent of the decibel value x.
--
-- Returns the amplitude equivalent of the decibel value x. Thus:
--
-- >  ampdb (x)  (no rate restriction)
--
-- csound doc: <http://csound.com/docs/manual/ampdb.html>
ampdb :: SigOrD a => a -> a
ampdb b1 = fromGE $ f <$> toGE b1
    where f a1 = opr1 "ampdb" a1

-- | 
-- Returns the amplitude equivalent (in 16-bit signed integer scale) of the full scale decibel (dB FS) value x.
--
-- Returns the amplitude equivalent of the full scale decibel (dB FS) value x. The logarithmic full scale decibel values will be converted to linear 16-bit signed integer values from â32,768 to +32,767.
--
-- >  ampdbfs (x)  (no rate restriction)
--
-- csound doc: <http://csound.com/docs/manual/ampdbfs.html>
ampdbfs :: SigOrD a => a -> a
ampdbfs b1 = fromGE $ f <$> toGE b1
    where f a1 = opr1 "ampdbfs" a1

-- | 
-- Returns the decibel equivalent of the raw amplitude x.
--
-- >  dbamp (x)  (init-rate or control-rate args only)
--
-- csound doc: <http://csound.com/docs/manual/dbamp.html>
dbamp :: SigOrD a => a -> a
dbamp b1 = fromGE $ f <$> toGE b1
    where f a1 = opr1k "dbamp" a1

-- | 
-- Returns the decibel equivalent of the raw amplitude x, relative to full scale amplitude.
--
-- Returns the decibel equivalent of the raw amplitude x, relative to full scale amplitude. Full scale is assumed to be 16 bit. New is Csound version 4.10.
--
-- >  dbfsamp (x)  (init-rate or control-rate args only)
--
-- csound doc: <http://csound.com/docs/manual/dbfsamp.html>
dbfsamp :: SigOrD a => a -> a
dbfsamp b1 = fromGE $ f <$> toGE b1
    where f a1 = opr1k "dbfsamp" a1

-- Random Functions.

-- | 
-- Returns a random number in a bi-polar range.
--
-- >  birnd (x) (init- or control-rate only)
--
-- csound doc: <http://csound.com/docs/manual/birnd.html>
birnd :: SigOrD a => a -> SE a
birnd b1 = fmap ( fromGE . return) $ SE $ (depT =<<) $ lift $ f <$> toGE b1
    where f a1 = opr1k "birnd" a1

-- | 
-- Returns a random number in a unipolar range at the rate given by the input argument.
--
-- >  rnd (x) (init- or control-rate only)
--
-- csound doc: <http://csound.com/docs/manual/rnd.html>
rnd :: SigOrD a => a -> SE a
rnd b1 = fmap ( fromGE . return) $ SE $ (depT =<<) $ lift $ f <$> toGE b1
    where f a1 = opr1k "rnd" a1

-- Opcode Equivalents of Functions.

-- | 
-- Safely divides two numbers.
--
-- > ares  divz  xa, xb, ksubst
-- > ires  divz  ia, ib, isubst
-- > kres  divz  ka, kb, ksubst
-- > ... divz (ka, kb, ksubst)... (no rate restriction)
--
-- csound doc: <http://csound.com/docs/manual/divz.html>
divz :: SigOrD a => a -> a -> a
divz b1 b2 = fromGE $ f <$> toGE b1 <*> toGE b2
    where f a1 a2 = opcs "divz" [(Ar,[Xr,Xr]),(Kr,[Kr,Kr]),(Ir,[Ir,Ir])] [a1,a2]

-- | 
-- Multiplies and accumulates a- and k-rate signals.
--
-- > ares  mac  ksig1, asig1 [, ksig2] [, asig2] [, ksig3] [, asig3] [...]
--
-- csound doc: <http://csound.com/docs/manual/mac.html>
mac ::  [Sig] -> Sig
mac b1 = Sig $ f <$> mapM unSig b1
    where f a1 = opcs "mac" [(Ar,[Kr,Ar,Kr,Ar,Kr] ++ (repeat Ar))] a1

-- | 
-- Multiply and accumulate a-rate signals only.
--
-- > ares  maca  asig1 , asig2 [, asig3] [, asig4] [, asig5] [...]
--
-- csound doc: <http://csound.com/docs/manual/maca.html>
maca ::  [Sig] -> Sig
maca b1 = Sig $ f <$> mapM unSig b1
    where f a1 = opcs "maca" [(Ar,(repeat Ar))] a1

-- | 
-- Efficiently evaluates a polynomial of arbitrary order.
--
-- The polynomial opcode calculates a polynomial with a single a-rate input variable.  The polynomial is a sum of any number of terms in the form kn*x^n where kn is the nth coefficient of the expression.  These coefficients are k-rate values.
--
-- > aout  polynomial  ain, k0 [, k1 [, k2 [...]]]
--
-- csound doc: <http://csound.com/docs/manual/polynomial.html>
polynomial ::  Sig -> [Sig] -> Sig
polynomial b1 b2 = Sig $ f <$> unSig b1 <*> mapM unSig b2
    where f a1 a2 = opcs "polynomial" [(Ar,[Ar] ++ (repeat Kr))] ([a1] ++ a2)

-- | 
-- Computes one argument to the power of another argument.
--
-- Computes xarg to the power of kpow (or ipow) and scales the result by inorm.
--
-- > ares  pow  aarg, kpow [, inorm]
-- > ires  pow  iarg, ipow [, inorm]
-- > kres  pow  karg, kpow [,
-- >         inorm]
-- > ires[]  pow  iarg[], ipow[] 
-- > kres[]  pow  karg[], kpow[]
-- > ires[]  pow  iarg[], ipow 
-- > kres[]  pow  karg[], kpow 
--
-- csound doc: <http://csound.com/docs/manual/pow.html>
pow ::  Sig -> Sig -> Sig
pow b1 b2 = Sig $ f <$> unSig b1 <*> unSig b2
    where f a1 a2 = opcs "pow" [(Ar,[Ar,Kr,Ir])
                               ,(Ir,[Ir,Ir,Ir])
                               ,(Kr,[Kr,Kr,Ir])
                               ,(Ir,[Ir,Ir])
                               ,(Kr,[Kr,Kr])
                               ,(Ir,[Ir,Ir])
                               ,(Kr,[Kr,Kr])] [a1,a2]

-- | 
-- Multiplies any number of a-rate signals.
--
-- > ares  product  asig1, asig2 [, asig3] [...]
--
-- csound doc: <http://csound.com/docs/manual/product.html>
product' ::  [Sig] -> Sig
product' b1 = Sig $ f <$> mapM unSig b1
    where f a1 = opcs "product" [(Ar,(repeat Ar))] a1

-- | 
-- Sums any number of a-rate signals, or array elements.
--
-- > ares  sum  asig1 [, asig2] [, asig3] [...]
-- > kres  sum  karr
-- > ires  sum  iarr
--
-- csound doc: <http://csound.com/docs/manual/sum.html>
sum' ::  [Sig] -> Sig
sum' b1 = Sig $ f <$> mapM unSig b1
    where f a1 = opcs "sum" [(Ar,(repeat Ar)),(Kr,[Kr]),(Ir,[Ir])] a1

-- | 
-- Returns an arctangent.
--
-- Returns the arctangent of iy/ix, ky/kx, or ay/ax.
--
-- > ares  taninv2  ay, ax
-- > ires  taninv2  iy, ix
-- > kres  taninv2  ky, kx
-- > ... taninv2 (ky, kx)... (no rate restriction)
--
-- csound doc: <http://csound.com/docs/manual/taninv2.html>
taninv2 :: SigOrD a => a -> a -> a
taninv2 b1 b2 = fromGE $ f <$> toGE b1 <*> toGE b2
    where f a1 a2 = opcs "taninv2" [(Ar,[Ar,Ar]),(Kr,[Kr,Kr]),(Ir,[Ir,Ir])] [a1,a2]