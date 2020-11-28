-- Zero delay filters (implemented in Csound by Steven Yi)
module Csound.Typed.Plugins.Zdf(
    -- One pole filters
    zdf1, zlp1, zhp1, zap1,

    -- Two pole filters
    zdf2, zlp, zbp, zhp, zdf2_notch, zbr,

    -- Ladder filter
    zladder,

    -- Four poles filters
    zdf4, zlp4, zbp4, zhp4,

    -- Eq-filters
    peakEq, highShelf, lowShelf
) where

import Data.Boolean
import Control.Monad.Trans.Class
import Control.Applicative

import Csound.Dynamic

import Csound.Typed.Types
import Csound.Typed.GlobalState
import qualified Csound.Typed.GlobalState.Elements as E(zdfPlugin)

-------------------------------------------------------------------------------
-- Haskell way (reorder arguments, split some funs)

-- zdf_1pole

zdf1 :: Sig -> Sig -> (Sig, Sig)
zdf1 cfq asig = zdf_1pole asig cfq

zlp1 :: Sig -> Sig -> Sig
zlp1 cfq asig = lows
    where (lows, _) = zdf_1pole asig cfq

zhp1 :: Sig -> Sig -> Sig
zhp1 cfq asig = highs
    where (_, highs) = zdf_1pole asig cfq

-- zdf_allpass_1pole

zap1 :: Sig -> Sig -> Sig
zap1 cfq asig = zdf_allpass_1pole asig cfq

-- zdf_2pole

-- outs: lp, bp, hp
zdf2 :: Sig -> Sig -> Sig -> (Sig, Sig, Sig)
zdf2 cfq q asig = zdf_2pole asig cfq q

zlp :: Sig -> Sig -> Sig -> Sig
zlp cfq q asig = lows
    where (lows, _, _) = zdf2 cfq q asig

zbp :: Sig -> Sig -> Sig -> Sig
zbp cfq q asig = mids
    where (_, mids, _) = zdf2 cfq q asig

zhp :: Sig -> Sig -> Sig -> Sig
zhp cfq q asig = highs
    where (_, _, highs) = zdf2 cfq q asig

zdf2_notch :: Sig -> Sig -> Sig -> (Sig, Sig, Sig, Sig)
zdf2_notch cfq q asig = zdf_2pole_notch asig cfq q

zbr cfq q asig = notch
    where (_, _, _, notch) = zdf2_notch cfq q asig

-- ladder

zladder :: Sig -> Sig -> Sig -> Sig
zladder cfq q asig = zdf_ladder asig cfq q

-- zdf_4pole

zdf4 ::  Sig -> Sig -> Sig -> (Sig, Sig, Sig, Sig, Sig, Sig)
zdf4 cfq q asig = zdf_4pole asig cfq q

zlp4 ::  Sig -> Sig -> Sig -> Sig
zlp4 cfq q asig = lows
    where (_, _, _, lows, _, _) = zdf4 cfq q asig

zbp4 ::  Sig -> Sig -> Sig -> Sig
zbp4 cfq q asig = mids
    where (_, _, _, _, mids, _) = zdf4 cfq q asig

zhp4 ::  Sig -> Sig -> Sig -> Sig
zhp4 cfq q asig = highs
    where (_, _, _, _, _, highs) = zdf4 cfq q asig

-- zdf_peak_eq
peakEq :: Sig -> Sig -> Sig -> Sig -> Sig
peakEq kcf kres kdB ain = zdf_peak_eq ain kcf kres kdB

-- zdf_high_shelf_eq
highShelf :: Sig -> Sig -> Sig -> Sig
highShelf kcf kres ain = zdf_high_shelf_eq ain kcf kres

-- zdf_low_shelf_eq
lowShelf :: Sig -> Sig -> Sig -> Sig
lowShelf kcf kres ain = zdf_low_shelf_eq ain kcf kres

-------------------------------------------------------------------------------
-- Steven implementation

-- 1-pole (6dB) lowpass/highpass filter
-- takes in a a-rate signal and cutoff value in frequency
--
-- xout alp, ahp
zdf_1pole :: Sig -> Sig -> (Sig, Sig)
zdf_1pole asig cfq = toTuple $ fmap ($ 2) $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq
    where f asig cfq = mopcs "zdf_1pole" ([Ar, Ar], [Ar, Ar]) [asig, cfq]

-- 1-pole allpass filter
-- takes in an a-rate signal and corner frequency where input
-- phase is shifted -90 degrees
zdf_allpass_1pole :: Sig -> Sig -> Sig
zdf_allpass_1pole asig cfq = fromGE $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq
    where f asig cfq = opcs "zdf_allpass_1pole" [(Ar, [Ar, Ar])] [asig, cfq]

-- 2-pole (12dB) lowpass/highpass/bandpass filter
-- takes in a a-rate signal, cutoff value in frequency, and
-- Q factor for resonance
--
-- xout alp, abp, ahp
zdf_2pole :: Sig -> Sig -> Sig -> (Sig, Sig, Sig)
zdf_2pole asig cfq q = toTuple $ fmap ($ 3) $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq <*> toGE q
    where f asig cfq q = mopcs "zdf_2pole" ([Ar, Ar, Ar], [Ar, Ar, Ar]) [asig, cfq, q]

-- 2-pole (12dB) lowpass/highpass/bandpass/notch filter
-- takes in a a-rate signal, cutoff value in frequency, and
-- Q factor for resonance
--
-- xout alp, abp, ahp, anotch
zdf_2pole_notch :: Sig -> Sig -> Sig -> (Sig, Sig, Sig, Sig)
zdf_2pole_notch asig cfq q = toTuple $ fmap ($ 4) $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq <*> toGE q
    where f asig cfq q = mopcs "zdf_2pole_notch" ([Ar, Ar, Ar, Ar], [Ar, Ar, Ar]) [asig, cfq, q]

-- moog ladder
--
-- opcode zdf_ladder, a, akk
--
-- ain, kcf, kres   xin
zdf_ladder :: Sig -> Sig -> Sig -> Sig
zdf_ladder asig cfq res = fromGE $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq <*> toGE res
    where f asig cfq res = opcs "zdf_ladder" [(Ar, [Ar, Ar, Ar])] [asig, cfq, res]

-- 4-pole
--
-- opcode zdf_4pole, aaaaaa, akk
--   ain, kcf, kres xin
--
-- xout alp2, abp2, ahp2, alp4, abl4, abp4
zdf_4pole :: Sig -> Sig -> Sig -> (Sig, Sig, Sig, Sig, Sig, Sig)
zdf_4pole asig cfq res = toTuple $ fmap ($ 6) $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq <*> toGE res
    where f asig cfq res = mopcs "zdf_4pole" ([Ar, Ar, Ar, Ar, Ar, Ar], [Ar, Ar, Ar]) [asig, cfq, res]

-- 4-pole
--
-- opcode zdf_4pole_hp, aaaaaa, akk
--   ain, kcf, kres xin
--
-- xout alp2, abp2, ahp2, alp4, abl4, abp4
zdf_4pole_hp :: Sig -> Sig -> Sig -> (Sig, Sig, Sig, Sig, Sig, Sig)
zdf_4pole_hp asig cfq res = toTuple $ fmap ($ 6) $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq <*> toGE res
    where f asig cfq res = mopcs "zdf_4pole_hp" ([Ar, Ar, Ar, Ar, Ar, Ar], [Ar, Ar, Ar]) [asig, cfq, res]

-- ;; TODO - implement
-- opcode zdf_peak_eq, a, akkk
-- ain, kcf, kres, kdB xin
zdf_peak_eq :: Sig -> Sig -> Sig -> Sig -> Sig
zdf_peak_eq ain kcf kres kdB = fromGE $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE ain <*> toGE kcf <*> toGE kres <*> toGE kdB
    where f ain kcf kres kdB = opcs "zdf_peak_eq" [(Ar, [Ar, Kr, Kr, Kr])] [ain, kcf, kres, kdB]

-- opcode zdf_high_shelf_eq, a, akk
--  ain, kcf, kdB xin
zdf_high_shelf_eq :: Sig -> Sig -> Sig -> Sig
zdf_high_shelf_eq asig cfq res = fromGE $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq <*> toGE res
    where f asig cfq res = opcs "zdf_high_shelf_eq" [(Ar, [Ar, Kr, Kr])] [asig, cfq, res]

-- opcode zdf_low_shelf_eq, a, akk
--  ain, kcf, kdB xin
zdf_low_shelf_eq :: Sig -> Sig -> Sig -> Sig
zdf_low_shelf_eq asig cfq res = fromGE $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq <*> toGE res
    where f asig cfq res = opcs "zdf_low_shelf_eq" [(Ar, [Ar, Kr, Kr])] [asig, cfq, res]
