{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language FlexibleInstances #-}
module Csound.SigSpace(
    SigSpace(..), BindSig(..), mul,
    cfd, cfds, cfdSpec, cfdsSpec, 
    wsum        
) where

import Control.Applicative

import Csound.Typed
import Csound.Typed.Opcode(pvscross)

-- | A class for easy way to process the outputs of the instruments.
class Num a => SigSpace a where
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
cfd :: SigSpace a => Sig -> a -> a -> a
cfd coeff a b = (1 - coeff) `mul` a + coeff `mul` b
  
genCfds :: a -> (Sig -> a -> a -> a) -> [Sig] -> [a] -> a
genCfds zero mixFun cs xs = case xs of
    []   -> zero
    a:as -> foldl (\x f -> f x) a $ zipWith mix' cs as 
    where mix' c a b = mixFun c b a
  
-- | Generic crossfade for n coefficients and n+1 signals.
--
-- > cfds coeffs sigs
cfds :: SigSpace a => [Sig] -> [a] -> a
cfds = genCfds 0 cfd

-- | Spectral crossfade.
cfdSpec :: Sig -> Spec -> Spec -> Spec
cfdSpec coeff a b = pvscross a b (1 - coeff) coeff

-- | Generic spectral crossfade.
cfdsSpec :: [Sig] -> [Spec] -> Spec
cfdsSpec = genCfds undefined cfdSpec

-- | Weighted sum.
wsum :: SigSpace a => [(Sig, a)] -> a
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

-----------------------------------------------------
-- numeric instances

-- Num

instance Num (Sig, Sig) where
    (a1, a2) + (b1, b2) = (a1 + b1, a2 + b2)
    (a1, a2) * (b1, b2) = (a1 * b1, a2 * b2)
    negate (a1, a2) = (negate a1, negate a2)

    fromInteger n = (fromInteger n, fromInteger n)
    signum (a1, a2) = (signum a1, signum a2)
    abs (a1, a2) = (abs a1, abs a2)

instance Num (Sig, Sig, Sig) where
    (a1, a2, a3) + (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)
    (a1, a2, a3) * (b1, b2, b3) = (a1 * b1, a2 * b2, a3 * b3)
    negate (a1, a2, a3) = (negate a1, negate a2, negate a3)

    fromInteger n = (fromInteger n, fromInteger n, fromInteger n)
    signum (a1, a2, a3) = (signum a1, signum a2, signum a3)
    abs (a1, a2, a3) = (abs a1, abs a2, abs a3)

instance Num (Sig, Sig, Sig, Sig) where
    (a1, a2, a3, a4) + (b1, b2, b3, b4) = (a1 + b1, a2 + b2, a3 + b3, a4 + b4)
    (a1, a2, a3, a4) * (b1, b2, b3, b4) = (a1 * b1, a2 * b2, a3 * b3, a4 * b4)
    negate (a1, a2, a3, a4) = (negate a1, negate a2, negate a3, negate a4)

    fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n)
    signum (a1, a2, a3, a4) = (signum a1, signum a2, signum a3, signum a4)
    abs (a1, a2, a3, a4) = (abs a1, abs a2, abs a3, abs a4)

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

