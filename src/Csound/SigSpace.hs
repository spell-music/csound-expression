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

-- | It applies an effect and balances the processed signal by original one.
bat :: At Sig a b => (Sig -> a) -> b -> AtOut Sig a b
bat f = at (\x -> mapSig ( `balance` x) $ f x)

-- | It applies an effect and balances the processed signal by original one.
-- Also it applies an effect and mixes the processed balanced signal with original one.
bmixAt :: MixAt Sig a b => Sig -> (Sig -> a) -> b -> AtOut Sig a b
bmixAt k f = mixAt k (\x -> mapSig ( `balance` x) $ f x)
