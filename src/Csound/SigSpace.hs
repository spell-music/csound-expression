{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language 
        TypeFamilies, 
        MultiParamTypeClasses, 
        FlexibleInstances, 
        FlexibleContexts #-}
module Csound.SigSpace(
    SigSpace(..), BindSig(..), mul, At(..), bat,
    cfd, cfd4, cfds, cfdSpec, cfdSpec4, cfdsSpec, 
    wsum        
) where

import Control.Monad
import Control.Applicative

import Csound.Typed
import Csound.Types
import Csound.Control.Gui(Source, mapSource)
import Csound.Typed.Opcode(pvscross, pvscale, pvsmix, balance)

-- | A class for easy way to process the outputs of the instruments.
class SigSpace a where
    mapSig  :: (Sig -> Sig)    -> a -> a

-- | A class for easy way to process the outputs of the instruments.
class SigSpace a => BindSig a where
    bindSig :: (Sig -> SE Sig) -> a -> SE a

-- | Scaling the sound.
mul :: SigSpace a => Sig -> a -> a
mul k = mapSig (k * )

-- | Crossfade.
--
-- > cfd coeff sig1 sig2
--
-- If coeff equals 0 then we get the first signal and if it equals 1 we get the second signal.
cfd :: (Num a, SigSpace a) => Sig -> a -> a -> a
cfd coeff a b = (1 - coeff) `mul` a + coeff `mul` b
  
genCfds :: a -> (Sig -> a -> a -> a) -> [Sig] -> [a] -> a
genCfds zero mixFun cs xs = case xs of
    []   -> zero
    a:as -> foldl (\x f -> f x) a $ zipWith mix' cs as 
    where mix' c a b = mixFun c b a

-- | Bilinear interpolation for four signals.
-- The signals are placed in the corners of the unit square.
-- The first two signals are the xy coordinates in the square. 
--
-- > cfd4 x y a b c d
--
-- * (0, 0) is for a
--
-- * (1, 0) is for b
--
-- * (1, 1) is for c
--
-- * (0, 1) is for d
cfd4 :: (Num a, SigSpace a) => Sig -> Sig -> a -> a -> a -> a -> a 
cfd4 x y a b c d = sum $ zipWith mul [(1 - x) * (1 - y), x * (1 - y) , x * y, (1 - x) * y] [a, b, c, d]
  
-- | Generic crossfade for n coefficients and n+1 signals.
--
-- > cfds coeffs sigs
cfds :: (Num a, SigSpace a) => [Sig] -> [a] -> a
cfds = genCfds 0 cfd

-- | Spectral crossfade.
cfdSpec :: Sig -> Spec -> Spec -> Spec
cfdSpec coeff a b = pvscross a b (1 - coeff) coeff

-- | Spectral bilinear crossfade (see @cfd4@).
cfdSpec4 :: Sig -> Sig -> Spec -> Spec -> Spec -> Spec -> Spec
cfdSpec4 x y a b c d = foldl1 pvsmix
    [ pvscale a ((1 - x) * (1 - y))
    , pvscale b (x * (1 - y))
    , pvscale c (x * y)
    , pvscale d ((1 - x) * y)
    ]

-- | Generic spectral crossfade.
cfdsSpec :: [Sig] -> [Spec] -> Spec
cfdsSpec = genCfds undefined cfdSpec

-- | Weighted sum.
wsum :: (Num a, SigSpace a) => [(Sig, a)] -> a
wsum = sum . fmap (uncurry mul)

instance SigSpace Sig   where  mapSig = id
instance BindSig  Sig   where  bindSig = id

instance SigSpace (Sig, Sig) where  mapSig  f (a1, a2) = (f a1, f a2)
instance BindSig  (Sig, Sig) where  bindSig f (a1, a2) = (,) <$> f a1 <*> f a2

instance SigSpace (Sig, Sig, Sig) where mapSig  f (a1, a2, a3) = (f a1, f a2, f a3)
instance BindSig  (Sig, Sig, Sig) where bindSig f (a1, a2, a3) = (,,) <$> f a1 <*> f a2 <*> f a3

instance SigSpace (Sig, Sig, Sig, Sig) where mapSig  f (a1, a2, a3, a4) = (f a1, f a2, f a3, f a4)
instance BindSig  (Sig, Sig, Sig, Sig) where bindSig f (a1, a2, a3, a4) = (,,,) <$> f a1 <*> f a2 <*> f a3 <*> f a4

instance SigSpace (SE Sig) where  mapSig  f = fmap (mapSig f)
instance BindSig  (SE Sig) where  bindSig f = fmap (bindSig f)

instance SigSpace (SE (Sig, Sig)) where mapSig  f = fmap (mapSig f)
instance BindSig  (SE (Sig, Sig)) where bindSig f = fmap (bindSig f)

instance SigSpace (SE (Sig, Sig, Sig)) where mapSig  f = fmap (mapSig f)
instance BindSig  (SE (Sig, Sig, Sig)) where bindSig f = fmap (bindSig f)

instance SigSpace (SE (Sig, Sig, Sig, Sig)) where mapSig  f = fmap (mapSig f)
instance BindSig  (SE (Sig, Sig, Sig, Sig)) where bindSig f = fmap (bindSig f)

instance SigSpace a => SigSpace (Source a) where
    mapSig f = mapSource (mapSig f)


-----------------------------------------------------
-- numeric instances

-- Num

instance Num (SE Sig) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate

    fromInteger = return . fromInteger
    signum = fmap signum
    abs = fmap abs

instance Num (SE (Sig, Sig)) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate

    fromInteger = return . fromInteger
    signum = fmap signum
    abs = fmap abs

instance Num (SE (Sig, Sig, Sig)) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate

    fromInteger = return . fromInteger
    signum = fmap signum
    abs = fmap abs

instance Num (SE (Sig, Sig, Sig, Sig)) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate

    fromInteger = return . fromInteger
    signum = fmap signum
    abs = fmap abs

instance Num (a -> Sig) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate

    fromInteger = return . fromInteger
    signum = fmap signum
    abs = fmap abs

instance Num (a -> (Sig, Sig)) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate

    fromInteger = return . fromInteger
    signum = fmap signum
    abs = fmap abs

instance Num (a -> (Sig, Sig, Sig)) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate

    fromInteger = return . fromInteger
    signum = fmap signum
    abs = fmap abs

instance Num (a -> (Sig, Sig, Sig, Sig)) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate

    fromInteger = return . fromInteger
    signum = fmap signum
    abs = fmap abs


instance Num (a -> SE Sig) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate

    fromInteger = return . fromInteger
    signum = fmap signum
    abs = fmap abs

instance Num (a -> SE (Sig, Sig)) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate

    fromInteger = return . fromInteger
    signum = fmap signum
    abs = fmap abs

instance Num (a -> SE (Sig, Sig, Sig)) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate

    fromInteger = return . fromInteger
    signum = fmap signum
    abs = fmap abs

instance Num (a -> SE (Sig, Sig, Sig, Sig)) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate

    fromInteger = return . fromInteger
    signum = fmap signum
    abs = fmap abs

-- Fractional

instance Fractional (Sig, Sig) where
    (a1, a2) / (b1, b2) = (a1 / b1, a2 / b2)
    fromRational a = (fromRational a, fromRational a)

instance Fractional (Sig, Sig, Sig) where
    (a1, a2, a3) / (b1, b2, b3) = (a1 / b1, a2 / b2, a3 / b3)
    fromRational a = (fromRational a, fromRational a, fromRational a)

instance Fractional (Sig, Sig, Sig, Sig) where
    (a1, a2, a3, a4) / (b1, b2, b3, b4) = (a1 / b1, a2 / b2, a3 / b3, a4 / b4)
    fromRational a = (fromRational a, fromRational a, fromRational a, fromRational a)

instance Fractional (SE Sig) where
    (/) = liftA2 (/)
    fromRational = return . fromRational

instance Fractional (SE (Sig, Sig)) where
    (/) = liftA2 (/)
    fromRational = return . fromRational

instance Fractional (SE (Sig, Sig, Sig)) where
    (/) = liftA2 (/)
    fromRational = return . fromRational

instance Fractional (SE (Sig, Sig, Sig, Sig)) where
    (/) = liftA2 (/)
    fromRational = return . fromRational

instance Fractional (a -> SE Sig) where
    (/) = liftA2 (/)
    fromRational = return . fromRational

instance Fractional (a -> SE (Sig, Sig)) where
    (/) = liftA2 (/)
    fromRational = return . fromRational

instance Fractional (a -> SE (Sig, Sig, Sig)) where
    (/) = liftA2 (/)
    fromRational = return . fromRational

instance Fractional (a -> SE (Sig, Sig, Sig, Sig)) where
    (/) = liftA2 (/)
    fromRational = return . fromRational

instance Fractional (a -> Sig) where
    (/) = liftA2 (/)
    fromRational = return . fromRational

instance Fractional (a -> (Sig, Sig)) where
    (/) = liftA2 (/)
    fromRational = return . fromRational

instance Fractional (a -> (Sig, Sig, Sig)) where
    (/) = liftA2 (/)
    fromRational = return . fromRational

instance Fractional (a -> (Sig, Sig, Sig, Sig)) where
    (/) = liftA2 (/)
    fromRational = return . fromRational

-----------------------------------------------------------------------
-----------------------------------------------------------------------

class SigSpace b => At a b c where
    type AtOut a b c :: *
    at :: (a -> b) -> c -> AtOut a b c

bat :: At Sig a b => (Sig -> a) -> b -> AtOut Sig a b
bat f = at (\x -> mapSig ( `balance` x) $ f x)

instance SigSpace a => At Sig Sig a where
    type AtOut Sig Sig a = a
    at f a = mapSig f a

------------------------------------------------------
-- for (Sig -> SE Sig)

instance At Sig (SE Sig) Sig where
    type AtOut Sig (SE Sig) Sig = SE Sig
    at f a = f a

instance At Sig (SE Sig) Sig2 where
    type AtOut Sig (SE Sig) Sig2 = SE Sig2
    at f a = bindSig f a

instance At Sig (SE Sig) Sig3 where
    type AtOut Sig (SE Sig) Sig3 = SE Sig3
    at f a = bindSig f a

instance At Sig (SE Sig) Sig4 where
    type AtOut Sig (SE Sig) Sig4 = SE Sig4
    at f a = bindSig f a

instance At Sig (SE Sig) (SE Sig) where
    type AtOut Sig (SE Sig) (SE Sig) = SE Sig
    at f a = join $ bindSig f a

instance At Sig (SE Sig) (SE Sig2) where
    type AtOut Sig (SE Sig) (SE Sig2) = SE Sig2
    at f a = join $ bindSig f a

instance At Sig (SE Sig) (SE Sig3) where
    type AtOut Sig (SE Sig) (SE Sig3) = SE Sig3
    at f a = join $ bindSig f a

instance At Sig (SE Sig) (SE Sig4) where
    type AtOut Sig (SE Sig) (SE Sig4) = SE Sig4
    at f a = join $ bindSig f a

-----------------------------------------------------
-- mono to stereo 

instance At Sig Sig2 Sig where
    type AtOut Sig Sig2 Sig = Sig2
    at f a = f a

instance At Sig Sig2 (SE Sig) where
    type AtOut Sig Sig2 (SE Sig) = SE Sig2
    at f a = fmap f a

instance At Sig Sig2 Sig2 where
    type AtOut Sig Sig2 Sig2 = Sig2
    at f a = 0.5 * (f (fst a) + f (snd a))

instance At Sig Sig2 (SE Sig2) where
    type AtOut Sig Sig2 (SE Sig2) = SE Sig2
    at f a = fmap (at f) a

---------------------------------------------------------   

instance (At Sig (SE Sig) a) => At Sig (SE Sig) (Source a) where
    type AtOut Sig (SE Sig) (Source a) = Source (AtOut Sig (SE Sig) a)
    at f a = mapSource (at f) a

---------------------------------------------------------   
-- Sig2 -> Sig2

fromMono a = (a, a)

instance At Sig2 Sig2 Sig where
    type AtOut Sig2 Sig2 Sig = Sig2
    at f a = f $ fromMono a

instance At Sig2 Sig2 Sig2 where
    type AtOut Sig2 Sig2 Sig2 = Sig2
    at f a = f a

instance At Sig2 Sig2 (SE Sig) where
    type AtOut Sig2 Sig2 (SE Sig) = SE Sig2
    at f a = fmap (f . fromMono) a

instance At Sig2 Sig2 (SE Sig2) where
    type AtOut Sig2 Sig2 (SE Sig2) = SE Sig2
    at f a = fmap f a

---------------------------------------------
-- Sig2 -> SE Sig2

instance At Sig2 (SE Sig2) Sig where
    type AtOut Sig2 (SE Sig2) Sig = SE Sig2
    at f a = f $ fromMono a

instance At Sig2 (SE Sig2) Sig2 where
    type AtOut Sig2 (SE Sig2) Sig2 = SE Sig2
    at f a = f a

instance At Sig2 (SE Sig2) (SE Sig) where
    type AtOut Sig2 (SE Sig2) (SE Sig) = SE Sig2
    at f a = (f . fromMono) =<< a

instance At Sig2 (SE Sig2) (SE Sig2) where
    type AtOut Sig2 (SE Sig2) (SE Sig2) = SE Sig2
    at f a = f =<< a

---------------------------------------------------------   

instance (At Sig2 Sig2 a) => At Sig2 Sig2 (Source a) where
    type AtOut Sig2 Sig2 (Source a) = Source (AtOut Sig2 Sig2 a)
    at f a = mapSource (at f) a

instance (At Sig2 (SE Sig2) a) => At Sig2 (SE Sig2) (Source a) where
    type AtOut Sig2 (SE Sig2) (Source a) = Source (AtOut Sig2 (SE Sig2) a)
    at f a = mapSource (at f) a

