module Csound.Typed.Plugins.Korg35(  
    linKorg_lp, linKorg_hp, korg_lp, korg_hp
) where

import Data.Boolean
import Control.Monad.Trans.Class
import Control.Applicative

import Csound.Dynamic

import Csound.Typed.Types
import Csound.Typed.GlobalState
import qualified Csound.Typed.GlobalState.Elements as E(korg35Plugin)

-- | Linear korg 35 low pass filter (12 dB). 
--
-- > linDiode centerFrequency resonance asig
--
-- resonance ranges in the interval [0, 1] and higher. 
-- self-resonance occurs at 1.
linKorg_lp :: Sig -> Sig -> Sig -> Sig
linKorg_lp cfq res ain = k35_lpf ain cfq (normReson res) 0 1

-- | Linear korg 35 high pass filter (6 dB). 
--
-- > linDiode centerFrequency resonance asig
--
-- resonance ranges in the interval [0, 1] and higher. 
-- self-resonance occurs at 1.
linKorg_hp :: Sig -> Sig -> Sig -> Sig
linKorg_hp cfq res ain = k35_hpf ain cfq (normReson res) 0 1


-- | Korg 35 low pass filter (12 dB). 
--
-- > diode saturation centerFrequency resonance asig
--
-- resonance ranges in the interval [0, 1] and higher. 
-- self-resonance occurs at 1.
--
-- saturation ranges from 1 and higher (typical value: 4)
korg_lp :: Sig -> Sig -> Sig -> Sig -> Sig
korg_lp ksaturation cfq res ain = k35_lpf ain cfq (normReson res) 1 ksaturation

-- | Korg 35 high pass filter (6 dB). 
--
-- > diode saturation centerFrequency resonance asig
--
-- resonance ranges in the interval [0, 1] and higher. 
-- self-resonance occurs at 1.
--
-- saturation ranges from 1 and higher (typical value: 4)
korg_hp :: Sig -> Sig -> Sig -> Sig -> Sig
korg_hp ksaturation cfq res ain = k35_hpf ain cfq (normReson res) 1 ksaturation

normReson :: Sig -> Sig
normReson res = res * 10

-------------------------------------------------------------------------------

-- 12db/oct low-pass filter based on Korg 35 module
-- (found in MS-10 and MS-20).
-- 
-- Based on code by Will Pirkle, presented in:
-- 
-- http://www.willpirkle.com/Downloads/AN-5Korg35_V3.pdf
-- 
-- [ARGS]
-- 
-- ain - audio input
-- acutoff - frequency of cutoff
-- kQ - filter Q [1, 10.0] (k35-lpf will clamp to boundaries)
-- knonlinear - use non-linear processing
-- ksaturation - saturation for tanh distortion
k35_lpf :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
k35_lpf ain acutoff kQ knonlinear ksaturation = fromGE $ do
    addUdoPlugin E.korg35Plugin
    f <$> toGE ain <*> toGE acutoff <*> toGE kQ <*> toGE knonlinear <*> toGE ksaturation
    where f ain acutoff kQ knonlinear ksaturation = opcs "k35_lpf" [(Ar, [Ar, Xr, Kr, Kr, Kr])] [ain, acutoff, kQ, knonlinear, ksaturation]


-- 6db/oct high-pass filter based on Korg 35 module
-- (found in MS-10 and MS-20).
-- 
-- Based on code by Will Pirkle, presented in:
-- 
-- http://www.willpirkle.com/Downloads/AN-7Korg35HPF_V2.pdf 
-- 
-- [ARGS]
-- 
-- ain - audio input
-- acutoff - frequency of cutoff
-- kQ - filter Q [1, 10.0] (k35_hpf will clamp to boundaries)
-- knonlinear - use non-linear processing
-- ksaturation - saturation for tanh distortion
k35_hpf :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
k35_hpf ain acutoff kQ knonlinear ksaturation = fromGE $ do
    addUdoPlugin E.korg35Plugin
    f <$> toGE ain <*> toGE acutoff <*> toGE kQ <*> toGE knonlinear <*> toGE ksaturation
    where f ain acutoff kQ knonlinear ksaturation = opcs "k35_hpf" [(Ar, [Ar, Xr, Kr, Kr, Kr])] [ain, acutoff, kQ, knonlinear, ksaturation]
