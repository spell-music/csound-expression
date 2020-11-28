{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language
        TypeFamilies,
        MultiParamTypeClasses,
        FlexibleInstances,
        FlexibleContexts,
        CPP #-}
module Csound.Typed.Types.SigSpace(
    SigSpace(..), BindSig(..), mul, mul', on, uon, At(..), MixAt(..),
    cfd, genCfds, cfd4, cfds,

    -- * Stereo sig-space
    SigSpace2(..), BindSig2(..), mul2, mul2',
) where

import Control.Monad
import Control.Applicative
import Csound.Typed.Types.Prim
import Csound.Typed.GlobalState.SE

-- | A class for easy way to process the outputs of the instruments.
class SigSpace a where
    mapSig  :: (Sig -> Sig)    -> a -> a

-- | A class for easy way to process the outputs of the instruments.
class SigSpace a => BindSig a where
    bindSig :: (Sig -> SE Sig) -> a -> SE a

-- | A class for easy way to process the outputs of the instruments.
class SigSpace2 a where
    mapSig2  :: (Sig2 -> Sig2)    -> a -> a

-- | A class for easy way to process the outputs of the instruments.
class SigSpace2 a => BindSig2 a where
    bindSig2 :: (Sig2 -> SE Sig2) -> a -> SE a


-- | Scaling the sound.
mul :: SigSpace a => Sig -> a -> a
mul k = mapSig (k * )

-- | Scaling the sound with effectful signal.
mul' :: BindSig a => SE Sig -> a -> SE a
mul' k = bindSig (\x -> fmap (* x) k)

-- | Scaling the sound with a pair.
mul2 :: SigSpace2 a => Sig2 -> a -> a
mul2 (ka, kb) = mapSig2 (\(a, b) -> (ka * a, kb * b))

-- | Scaling the sound with effectful pair of signals.
mul2' :: BindSig2 a => SE Sig2 -> a -> SE a
mul2' k = bindSig2 (\(xa, xb) -> fmap (\(ka, kb) -> (ka * xa, kb * xb)) k)

-- rescaling

-- | Rescaling of the bipolar signal (-1, 1) -> (a, b)
--
-- > on a b biSig
on :: SigSpace a => Sig -> Sig -> a -> a
on a b x = uon a b $ mapSig unipolar x
    where unipolar a = 0.5 + 0.5 * a

-- | Rescaling of the unipolar signal (0, 1) -> (a, b)
--
-- > on a b uniSig
uon :: SigSpace a => Sig -> Sig -> a -> a
uon a b = mapSig (\x -> a + (b - a) * x)

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


instance SigSpace Sig   where  mapSig = id
instance BindSig  Sig   where  bindSig = id

#if __GLASGOW_HASKELL__ >= 710
instance (SigSpace a1, SigSpace a2) => SigSpace (a1, a2) where  mapSig  f (a1, a2) = (mapSig f a1, mapSig f a2)
instance (BindSig a1, BindSig a2) => BindSig  (a1, a2) where  bindSig f (a1, a2) = (,) <$> bindSig f a1 <*> bindSig f a2

instance (SigSpace a1, SigSpace a2, SigSpace a3) => SigSpace (a1, a2, a3) where mapSig  f (a1, a2, a3) = (mapSig f a1, mapSig f a2, mapSig f a3)
instance (BindSig a1, BindSig a2, BindSig a3) => BindSig  (a1, a2, a3) where bindSig f (a1, a2, a3) = (,,) <$> bindSig f a1 <*> bindSig f a2 <*> bindSig f a3

instance (SigSpace a1, SigSpace a2, SigSpace a3, SigSpace a4) => SigSpace (a1, a2, a3, a4) where mapSig  f (a1, a2, a3, a4) = (mapSig f a1, mapSig f a2, mapSig f a3, mapSig f a4)
instance (BindSig a1, BindSig a2, BindSig a3, BindSig a4) => BindSig  (a1, a2, a3, a4) where bindSig f (a1, a2, a3, a4) = (,,,) <$> bindSig f a1 <*> bindSig f a2 <*> bindSig f a3 <*> bindSig f a4

instance (SigSpace a1, SigSpace a2, SigSpace a3, SigSpace a4, SigSpace a5) => SigSpace (a1, a2, a3, a4, a5) where mapSig  f (a1, a2, a3, a4, a5) = (mapSig f a1, mapSig f a2, mapSig f a3, mapSig f a4, mapSig f a5)
instance (BindSig a1, BindSig a2, BindSig a3, BindSig a4, BindSig a5) => BindSig  (a1, a2, a3, a4, a5) where bindSig f (a1, a2, a3, a4, a5) = (,,,,) <$> bindSig f a1 <*> bindSig f a2 <*> bindSig f a3 <*> bindSig f a4 <*> bindSig f a5

instance (SigSpace a1, SigSpace a2, SigSpace a3, SigSpace a4, SigSpace a5, SigSpace a6) => SigSpace (a1, a2, a3, a4, a5, a6) where mapSig  f (a1, a2, a3, a4, a5, a6) = (mapSig f a1, mapSig f a2, mapSig f a3, mapSig f a4, mapSig f a5, mapSig f a6)
instance (BindSig a1, BindSig a2, BindSig a3, BindSig a4, BindSig a5, BindSig a6) => BindSig  (a1, a2, a3, a4, a5, a6) where bindSig f (a1, a2, a3, a4, a5, a6) = (,,,,,) <$> bindSig f a1 <*> bindSig f a2 <*> bindSig f a3 <*> bindSig f a4 <*> bindSig f a5 <*> bindSig f a6

instance (SigSpace a1, SigSpace a2, SigSpace a3, SigSpace a4, SigSpace a5, SigSpace a6, SigSpace a7) => SigSpace (a1, a2, a3, a4, a5, a6, a7) where mapSig  f (a1, a2, a3, a4, a5, a6, a7) = (mapSig f a1, mapSig f a2, mapSig f a3, mapSig f a4, mapSig f a5, mapSig f a6, mapSig f a7)
instance (BindSig a1, BindSig a2, BindSig a3, BindSig a4, BindSig a5, BindSig a6, BindSig a7) => BindSig  (a1, a2, a3, a4, a5, a6, a7) where bindSig f (a1, a2, a3, a4, a5, a6, a7) = (,,,,,,) <$> bindSig f a1 <*> bindSig f a2 <*> bindSig f a3 <*> bindSig f a4 <*> bindSig f a5 <*> bindSig f a6 <*> bindSig f a7

instance (SigSpace a1, SigSpace a2, SigSpace a3, SigSpace a4, SigSpace a5, SigSpace a6, SigSpace a7, SigSpace a8) => SigSpace (a1, a2, a3, a4, a5, a6, a7, a8) where mapSig  f (a1, a2, a3, a4, a5, a6, a7, a8) = (mapSig f a1, mapSig f a2, mapSig f a3, mapSig f a4, mapSig f a5, mapSig f a6, mapSig f a7, mapSig f a8)
instance (BindSig a1, BindSig a2, BindSig a3, BindSig a4, BindSig a5, BindSig a6, BindSig a7, BindSig a8) => BindSig  (a1, a2, a3, a4, a5, a6, a7, a8) where bindSig f (a1, a2, a3, a4, a5, a6, a7, a8) = (,,,,,,,) <$> bindSig f a1 <*> bindSig f a2 <*> bindSig f a3 <*> bindSig f a4 <*> bindSig f a5 <*> bindSig f a6 <*> bindSig f a7 <*> bindSig f a8

instance {-# OVERLAPPABLE #-} SigSpace a => SigSpace (SE a) where  mapSig  f = fmap (mapSig f)
instance {-# OVERLAPPABLE #-} BindSig a  => BindSig  (SE a) where  bindSig f = fmap (bindSig f)
#endif

#if __GLASGOW_HASKELL__ < 710

instance SigSpace (Sig, Sig) where  mapSig  f (a1, a2) = (mapSig f a1, mapSig f a2)
instance BindSig  (Sig, Sig) where  bindSig f (a1, a2) = (,) <$> bindSig f a1 <*> bindSig f a2

instance SigSpace (Sig, Sig, Sig) where mapSig  f (a1, a2, a3) = (mapSig f a1, mapSig f a2, mapSig f a3)
instance BindSig  (Sig, Sig, Sig) where bindSig f (a1, a2, a3) = (,,) <$> bindSig f a1 <*> bindSig f a2 <*> bindSig f a3

instance SigSpace (Sig, Sig, Sig, Sig) where mapSig  f (a1, a2, a3, a4) = (mapSig f a1, mapSig f a2, mapSig f a3, mapSig f a4)
instance BindSig  (Sig, Sig, Sig, Sig) where bindSig f (a1, a2, a3, a4) = (,,,) <$> bindSig f a1 <*> bindSig f a2 <*> bindSig f a3 <*> bindSig f a4

instance SigSpace (Sig, Sig, Sig, Sig, Sig) where mapSig  f (a1, a2, a3, a4, a5) = (mapSig f a1, mapSig f a2, mapSig f a3, mapSig f a4, mapSig f a5)
instance BindSig  (Sig, Sig, Sig, Sig, Sig) where bindSig f (a1, a2, a3, a4, a5) = (,,,,) <$> bindSig f a1 <*> bindSig f a2 <*> bindSig f a3 <*> bindSig f a4 <*> bindSig f a5

instance SigSpace (Sig, Sig, Sig, Sig, Sig, Sig) where mapSig  f (a1, a2, a3, a4, a5, a6) = (mapSig f a1, mapSig f a2, mapSig f a3, mapSig f a4, mapSig f a5, mapSig f a6)
instance BindSig  (Sig, Sig, Sig, Sig, Sig, Sig) where bindSig f (a1, a2, a3, a4, a5, a6) = (,,,,,) <$> bindSig f a1 <*> bindSig f a2 <*> bindSig f a3 <*> bindSig f a4 <*> bindSig f a5 <*> bindSig f a6

instance SigSpace (Sig, Sig, Sig, Sig, Sig, Sig, Sig) where mapSig  f (a1, a2, a3, a4, a5, a6, a7) = (mapSig f a1, mapSig f a2, mapSig f a3, mapSig f a4, mapSig f a5, mapSig f a6, mapSig f a7)
instance BindSig  (Sig, Sig, Sig, Sig, Sig, Sig, Sig) where bindSig f (a1, a2, a3, a4, a5, a6, a7) = (,,,,,,) <$> bindSig f a1 <*> bindSig f a2 <*> bindSig f a3 <*> bindSig f a4 <*> bindSig f a5 <*> bindSig f a6 <*> bindSig f a7

instance SigSpace (Sig, Sig, Sig, Sig, Sig, Sig, Sig, Sig) where mapSig  f (a1, a2, a3, a4, a5, a6, a7, a8) = (mapSig f a1, mapSig f a2, mapSig f a3, mapSig f a4, mapSig f a5, mapSig f a6, mapSig f a7, mapSig f a8)
instance BindSig  (Sig, Sig, Sig, Sig, Sig, Sig, Sig, Sig) where bindSig f (a1, a2, a3, a4, a5, a6, a7, a8) = (,,,,,,,) <$> bindSig f a1 <*> bindSig f a2 <*> bindSig f a3 <*> bindSig f a4 <*> bindSig f a5 <*> bindSig f a6 <*> bindSig f a7 <*> bindSig f a8

instance SigSpace (SE Sig) where  mapSig  f = fmap (mapSig f)
instance BindSig  (SE Sig) where  bindSig f = fmap (bindSig f)

instance SigSpace (SE (Sig, Sig)) where  mapSig  f = fmap (mapSig f)
instance BindSig  (SE (Sig, Sig)) where  bindSig f = fmap (bindSig f)

instance SigSpace (SE (Sig, Sig, Sig)) where  mapSig  f = fmap (mapSig f)
instance BindSig  (SE (Sig, Sig, Sig)) where  bindSig f = fmap (bindSig f)

instance SigSpace (SE (Sig, Sig, Sig, Sig)) where  mapSig  f = fmap (mapSig f)
instance BindSig  (SE (Sig, Sig, Sig, Sig)) where  bindSig f = fmap (bindSig f)

instance SigSpace (SE (Sig, Sig, Sig, Sig, Sig)) where  mapSig  f = fmap (mapSig f)
instance BindSig  (SE (Sig, Sig, Sig, Sig, Sig)) where  bindSig f = fmap (bindSig f)

instance SigSpace (SE (Sig, Sig, Sig, Sig, Sig, Sig)) where  mapSig  f = fmap (mapSig f)
instance BindSig  (SE (Sig, Sig, Sig, Sig, Sig, Sig)) where  bindSig f = fmap (bindSig f)

instance SigSpace (SE (Sig, Sig, Sig, Sig, Sig, Sig, Sig)) where  mapSig  f = fmap (mapSig f)
instance BindSig  (SE (Sig, Sig, Sig, Sig, Sig, Sig, Sig)) where  bindSig f = fmap (bindSig f)

instance SigSpace (SE (Sig, Sig, Sig, Sig, Sig, Sig, Sig, Sig)) where  mapSig  f = fmap (mapSig f)
instance BindSig  (SE (Sig, Sig, Sig, Sig, Sig, Sig, Sig, Sig)) where  bindSig f = fmap (bindSig f)
#endif

----------------------------------------------------------------------------------------------------------

-- | Converts stereosignal to mono with function mean.
toMono :: (Sig, Sig) -> Sig
toMono (a, b) = 0.5 * a + 0.5 * b

instance SigSpace2 Sig   where  mapSig2  f a = toMono $ f (a, a)
instance BindSig2  Sig   where  bindSig2 f a = fmap toMono $ f (a, a)

instance SigSpace2 (Sig, Sig) where  mapSig2  = id
instance BindSig2  (Sig, Sig) where  bindSig2 = id

instance SigSpace2 (Sig, Sig, Sig) where
    mapSig2  f (a1, a2, a3) = (b1, b2, toMono (b3, b4))
        where
            (b1, b2, b3, b4) = mapSig2 f (a1, a2, a3, a4)
            a4 = a3

instance BindSig2  (Sig, Sig, Sig) where
    bindSig2 f (a1, a2, a3) = do
        (b1, b2, b3, b4) <- bindSig2 f (a1, a2, a3, a4)
        return (b1, b2, toMono (b3, b4))
        where
            a4 = a3

instance SigSpace2 (Sig, Sig, Sig, Sig) where
    mapSig2  f (a1, a2, a3, a4) = (b1, b2, b3, b4)
        where
            (b1, b2) = f (a1, a2)
            (b3, b4) = f (a3, a4)

instance BindSig2  (Sig, Sig, Sig, Sig) where
    bindSig2 f (a1, a2, a3, a4) = do
            (b1, b2) <- f (a1, a2)
            (b3, b4) <- f (a3, a4)
            return (b1, b2, b3, b4)

instance SigSpace2 (Sig, Sig, Sig, Sig, Sig) where
    mapSig2  f (a1, a2, a3, a4, a5) = (b1, b2, b3, b4, toMono (b5, b6))
        where
            (b1, b2, b3, b4, b5, b6) = mapSig2 f (a1, a2, a3, a4, a5, a6)
            a6 = a5

instance BindSig2 (Sig, Sig, Sig, Sig, Sig) where
    bindSig2 f (a1, a2, a3, a4, a5) = do
        (b1, b2, b3, b4, b5, b6) <- bindSig2 f (a1, a2, a3, a4, a5, a6)
        return (b1, b2, b3, b4, toMono (b5, b6))
        where
            a6 = a5

instance SigSpace2 (Sig, Sig, Sig, Sig, Sig, Sig) where
    mapSig2  f (a1, a2, a3, a4, a5, a6) = (b1, b2, b3, b4, b5, b6)
        where
            (b1, b2, b3, b4) = mapSig2 f (a1, a2, a3, a4)
            (b5, b6) = f (a5, a6)

instance BindSig2  (Sig, Sig, Sig, Sig, Sig, Sig) where
    bindSig2 f (a1, a2, a3, a4, a5, a6) = do
        (b1, b2, b3, b4) <- bindSig2 f (a1, a2, a3, a4)
        (b5, b6) <- f (a5, a6)
        return (b1, b2, b3, b4, b5, b6)

instance SigSpace2 (Sig, Sig, Sig, Sig, Sig, Sig, Sig) where
    mapSig2  f (a1, a2, a3, a4, a5, a6, a7) = (b1, b2, b3, b4, b5, b6, toMono (b7, b8))
        where
            (b1, b2, b3, b4, b5, b6, b7, b8) = mapSig2 f (a1, a2, a3, a4, a5, a6, a7, a8)
            a8 = a7

instance BindSig2  (Sig, Sig, Sig, Sig, Sig, Sig, Sig) where
    bindSig2 f (a1, a2, a3, a4, a5, a6, a7) = do
        (b1, b2, b3, b4, b5, b6, b7, b8) <- bindSig2 f (a1, a2, a3, a4, a5, a6, a7, a8)
        return (b1, b2, b3, b4, b5, b6, toMono (b7, b8))
        where
            a8 = a7

instance SigSpace2 (Sig, Sig, Sig, Sig, Sig, Sig, Sig, Sig) where
    mapSig2  f (a1, a2, a3, a4, a5, a6, a7, a8) = (b1, b2, b3, b4, b5, b6, b7, b8)
        where
            (b1, b2, b3, b4, b5, b6) = mapSig2 f (a1, a2, a3, a4, a5, a6)
            (b7, b8) = f (a7, a8)

instance BindSig2  (Sig, Sig, Sig, Sig, Sig, Sig, Sig, Sig) where
    bindSig2 f (a1, a2, a3, a4, a5, a6, a7, a8) = do
        (b1, b2, b3, b4, b5, b6) <- bindSig2 f (a1, a2, a3, a4, a5, a6)
        (b7, b8) <- f (a7, a8)
        return (b1, b2, b3, b4, b5, b6, b7, b8)

instance SigSpace2 (Sig2, Sig2) where  mapSig2  f (a1, a2) = (mapSig2 f a1, mapSig2 f a2)
instance BindSig2  (Sig2, Sig2) where  bindSig2 f (a1, a2) = (,) <$> bindSig2 f a1 <*> bindSig2 f a2

instance SigSpace2 (Sig2, Sig2, Sig2) where  mapSig2  f (a1, a2, a3) = (mapSig2 f a1, mapSig2 f a2, mapSig2 f a3)
instance BindSig2  (Sig2, Sig2, Sig2) where  bindSig2 f (a1, a2, a3) = (,,) <$> bindSig2 f a1 <*> bindSig2 f a2 <*> bindSig2 f a3

instance SigSpace2 (Sig2, Sig2, Sig2, Sig2) where  mapSig2  f (a1, a2, a3, a4) = (mapSig2 f a1, mapSig2 f a2, mapSig2 f a3, mapSig2 f a4)
instance BindSig2  (Sig2, Sig2, Sig2, Sig2) where  bindSig2 f (a1, a2, a3, a4) = (,,,) <$> bindSig2 f a1 <*> bindSig2 f a2 <*> bindSig2 f a3 <*> bindSig2 f a4

instance SigSpace2 (Sig2, Sig2, Sig2, Sig2, Sig2) where  mapSig2  f (a1, a2, a3, a4, a5) = (mapSig2 f a1, mapSig2 f a2, mapSig2 f a3, mapSig2 f a4, mapSig2 f a5)
instance BindSig2  (Sig2, Sig2, Sig2, Sig2, Sig2) where  bindSig2 f (a1, a2, a3, a4, a5) = (,,,,) <$> bindSig2 f a1 <*> bindSig2 f a2 <*> bindSig2 f a3 <*> bindSig2 f a4 <*> bindSig2 f a5

instance SigSpace2 (Sig2, Sig2, Sig2, Sig2, Sig2, Sig2) where  mapSig2  f (a1, a2, a3, a4, a5, a6) = (mapSig2 f a1, mapSig2 f a2, mapSig2 f a3, mapSig2 f a4, mapSig2 f a5, mapSig2 f a6)
instance BindSig2  (Sig2, Sig2, Sig2, Sig2, Sig2, Sig2) where  bindSig2 f (a1, a2, a3, a4, a5, a6) = (,,,,,) <$> bindSig2 f a1 <*> bindSig2 f a2 <*> bindSig2 f a3 <*> bindSig2 f a4 <*> bindSig2 f a5  <*> bindSig2 f a6

instance SigSpace2 (Sig2, Sig2, Sig2, Sig2, Sig2, Sig2, Sig2) where  mapSig2  f (a1, a2, a3, a4, a5, a6, a7) = (mapSig2 f a1, mapSig2 f a2, mapSig2 f a3, mapSig2 f a4, mapSig2 f a5, mapSig2 f a6, mapSig2 f a7)
instance BindSig2  (Sig2, Sig2, Sig2, Sig2, Sig2, Sig2, Sig2) where  bindSig2 f (a1, a2, a3, a4, a5, a6, a7) = (,,,,,,) <$> bindSig2 f a1 <*> bindSig2 f a2 <*> bindSig2 f a3 <*> bindSig2 f a4 <*> bindSig2 f a5  <*> bindSig2 f a6 <*> bindSig2 f a7

instance SigSpace2 (Sig2, Sig2, Sig2, Sig2, Sig2, Sig2, Sig2, Sig2) where  mapSig2  f (a1, a2, a3, a4, a5, a6, a7, a8) = (mapSig2 f a1, mapSig2 f a2, mapSig2 f a3, mapSig2 f a4, mapSig2 f a5, mapSig2 f a6, mapSig2 f a7, mapSig2 f a8)
instance BindSig2  (Sig2, Sig2, Sig2, Sig2, Sig2, Sig2, Sig2, Sig2) where  bindSig2 f (a1, a2, a3, a4, a5, a6, a7, a8) = (,,,,,,,) <$> bindSig2 f a1 <*> bindSig2 f a2 <*> bindSig2 f a3 <*> bindSig2 f a4 <*> bindSig2 f a5  <*> bindSig2 f a6 <*> bindSig2 f a7 <*> bindSig2 f a8

instance SigSpace2 (Sig8, Sig8) where  mapSig2  f (a1, a2) = (mapSig2 f a1, mapSig2 f a2)
instance BindSig2  (Sig8, Sig8) where  bindSig2 f (a1, a2) = (,) <$> bindSig2 f a1 <*> bindSig2 f a2

instance SigSpace2 (Sig8, Sig8, Sig8, Sig8) where  mapSig2  f (a1, a2, a3, a4) = (mapSig2 f a1, mapSig2 f a2, mapSig2 f a3, mapSig2 f a4)
instance BindSig2  (Sig8, Sig8, Sig8, Sig8) where  bindSig2 f (a1, a2, a3, a4) = (,,,) <$> bindSig2 f a1 <*> bindSig2 f a2 <*> bindSig2 f a3 <*> bindSig2 f a4

instance SigSpace2 (SE Sig) where  mapSig2  f = fmap (mapSig2 f)
instance BindSig2  (SE Sig) where  bindSig2 f = fmap (bindSig2 f)

instance SigSpace2 (SE (Sig, Sig)) where mapSig2  f = fmap (mapSig2 f)
instance BindSig2  (SE (Sig, Sig)) where bindSig2 f = fmap (bindSig2 f)

instance SigSpace2 (SE (Sig, Sig, Sig)) where mapSig2  f = fmap (mapSig2 f)
instance BindSig2  (SE (Sig, Sig, Sig)) where bindSig2 f = fmap (bindSig2 f)

instance SigSpace2 (SE (Sig, Sig, Sig, Sig)) where mapSig2  f = fmap (mapSig2 f)
instance BindSig2  (SE (Sig, Sig, Sig, Sig)) where bindSig2 f = fmap (bindSig2 f)

----------------------------------------------------------------------------------------------------------
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

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- MixAt

-- | It applies an effect and mixes the processed signal with original one.
-- The first argument is for proportion of dry/wet (original/processed).
-- It's like @at@ but it allows to balance processed signal with original one.
class (SigSpace b, At a b c) => MixAt a b c where
    mixAt :: Sig -> (a -> b) -> c -> AtOut a b c

---------------------------------------------------

instance SigSpace a => MixAt Sig Sig a where
    mixAt k f a = mapSig (\x -> cfd k x (f x)) a

------------------------------------------------------
-- for (Sig -> SE Sig)

instance MixAt Sig (SE Sig) Sig where
    mixAt k f dry = do
        wet <- f dry
        return $ cfd k dry wet

instance MixAt Sig (SE Sig) Sig2 where
    mixAt k f (dry1, dry2) = do
        wet1 <- f dry1
        wet2 <- f dry2
        return $ cfd k (dry1, dry2) (wet1, wet2)

instance MixAt Sig (SE Sig) Sig3 where
    mixAt k f (dry1, dry2, dry3) = do
        wet1 <- f dry1
        wet2 <- f dry2
        wet3 <- f dry3
        return $ cfd k (dry1, dry2, dry3) (wet1, wet2, wet3)

instance MixAt Sig (SE Sig) Sig4 where
    mixAt k f (dry1, dry2, dry3, dry4) = do
        wet1 <- f dry1
        wet2 <- f dry2
        wet3 <- f dry3
        wet4 <- f dry4
        return $ cfd k (dry1, dry2, dry3, dry4) (wet1, wet2, wet3, wet4)

instance MixAt Sig (SE Sig) (SE Sig) where
    mixAt k f dry = do
        dry1 <- dry
        wet1 <- f dry1
        return $ cfd k dry1 wet1

instance MixAt Sig (SE Sig) (SE Sig2) where
    mixAt k f dry = do
        (dry1, dry2) <- dry
        wet1 <- f dry1
        wet2 <- f dry2
        return $ cfd k (dry1, dry2) (wet1, wet2)

instance MixAt Sig (SE Sig) (SE Sig3) where
    mixAt k f dry = do
        (dry1, dry2, dry3) <- dry
        wet1 <- f dry1
        wet2 <- f dry2
        wet3 <- f dry3
        return $ cfd k (dry1, dry2, dry3) (wet1, wet2, wet3)

instance MixAt Sig (SE Sig) (SE Sig4) where
    mixAt k f dry = do
        (dry1, dry2, dry3, dry4) <- dry
        wet1 <- f dry1
        wet2 <- f dry2
        wet3 <- f dry3
        wet4 <- f dry4
        return $ cfd k (dry1, dry2, dry3, dry4) (wet1, wet2, wet3, wet4)

-----------------------------------------------------
-- mono to stereo

instance MixAt Sig Sig2 Sig where
    mixAt k f dry = cfd k (dry, dry) wet
        where wet = f dry

instance MixAt Sig Sig2 (SE Sig) where
    mixAt k f dry = fmap (\x -> cfd k (x, x) (f x)) dry

instance MixAt Sig Sig2 Sig2 where
    mixAt k f dry = cfd k dry wet
        where wet = 0.5 * (f (fst dry) + f (snd dry))

instance MixAt Sig Sig2 (SE Sig2) where
    mixAt k f dry = do
        (dry1, dry2) <- dry
        let wet = 0.5 * (f dry1 + f dry2)
        return $ cfd k (dry1, dry2) wet

---------------------------------------------------------

---------------------------------------------------------
-- Sig2 -> Sig2

instance MixAt Sig2 Sig2 Sig where
    mixAt k f dry1 = cfd k dry wet
        where
            dry = fromMono dry1
            wet = f dry

instance MixAt Sig2 Sig2 Sig2 where
    mixAt k f dry = cfd k dry wet
        where
            wet = f dry

instance MixAt Sig2 Sig2 (SE Sig) where
    mixAt k f dry1 = do
        dry <- fmap fromMono dry1
        let wet = f dry
        return $ cfd k dry wet

instance MixAt Sig2 Sig2 (SE Sig2) where
    mixAt k f drySe = do
        dry <- drySe
        let wet = f dry
        return $ cfd k dry wet


---------------------------------------------
-- Sig2 -> SE Sig2

instance MixAt Sig2 (SE Sig2) Sig where
    mixAt k f dry1 = do
        wet <- f dry
        return $ cfd k dry wet
        where
            dry = fromMono dry1

instance MixAt Sig2 (SE Sig2) Sig2 where
    mixAt k f dry = do
        wet <- f dry
        return $ cfd k dry wet

instance MixAt Sig2 (SE Sig2) (SE Sig) where
    mixAt k f dry1 = do
        dry <- fmap fromMono dry1
        wet <- f dry
        return $ cfd k dry wet

instance MixAt Sig2 (SE Sig2) (SE Sig2) where
    mixAt k f drySe = do
        dry <- drySe
        wet <- f dry
        return $ cfd k dry wet
