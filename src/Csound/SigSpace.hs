{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language 
        TypeFamilies, 
        MultiParamTypeClasses, 
        FlexibleInstances, 
        FlexibleContexts #-}
module Csound.SigSpace(
    SigSpace(..), BindSig(..), mul, mul', on, uon, At(..), MixAt(..), bat, bmixAt,
    cfd, cfd4, cfds, cfdSpec, cfdSpec4, cfdsSpec, 
    wsum        
) where

import Control.Monad
import Control.Applicative

import Csound.Typed
import Csound.Types
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

-- | Scaling the sound with effectful signal.
mul' :: BindSig a => SE Sig -> a -> SE a
mul' k = bindSig (\x -> fmap (* x) k)

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

-- | It applies an effect and balances the processed signal by original one.
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

-- | It applies an effect and balances the processed signal by original one.
-- Also it applies an effect and mixes the processed balanced signal with original one.
bmixAt :: MixAt Sig a b => Sig -> (Sig -> a) -> b -> AtOut Sig a b
bmixAt k f = mixAt k (\x -> mapSig ( `balance` x) $ f x)

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
