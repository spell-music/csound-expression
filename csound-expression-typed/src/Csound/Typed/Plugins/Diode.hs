module Csound.Typed.Plugins.Diode(  
    diode, linDiode, noNormDiode
) where

import Data.Boolean
import Control.Monad.Trans.Class
import Control.Applicative

import Csound.Dynamic

import Csound.Typed.Types
import Csound.Typed.GlobalState
import qualified Csound.Typed.GlobalState.Elements as E(diodePlugin)

-- | Linear diode ladder filter. 
--
-- > linDiode centerFrequency resonance asig
--
-- resonance ranges in the interval [0, 1] and higher. 
-- self-resonance occurs at 1.
linDiode :: Sig -> Sig -> Sig -> Sig
linDiode cfq res ain = diodeLadder ain cfq (normReson res) 0 1

-- | Non-Linear normalized diode ladder filter. 
--
-- > diode saturation centerFrequency resonance asig
--
-- resonance ranges in the interval [0, 1] and higher. 
-- self-resonance occurs at 1.
--
-- saturation ranges from 1 and higher (typical value: 4)
diode :: Sig -> Sig -> Sig -> Sig -> Sig
diode ksaturation cfq res ain = diodeLadder ain cfq (normReson res) 1 ksaturation

-- | Non-Linear not normalized diode ladder filter. 
--
-- > noNormDiode saturation centerFrequency resonance asig
--
-- resonance ranges in the interval [0, 1] and higher. 
-- self-resonance occurs at 1.
--
-- saturation ranges from 1 and higher (typical value: 4)
noNormDiode :: Sig -> Sig -> Sig -> Sig -> Sig
noNormDiode ksaturation cfq res ain = diodeLadder ain cfq (normReson res) 2 ksaturation

normReson :: Sig -> Sig
normReson res = res * 17

-------------------------------------------------------------------------------

-- | Diode Ladder Filter
-- 
-- Based on code by Will Pirkle, presented in:
--
-- http://www.willpirkle.com/Downloads/AN-6DiodeLadderFilter.pdf
-- 
-- and in his book "Designing software synthesizer plug-ins in C++ : for 
-- RackAFX, VST3, and Audio Units"
--
-- UDO version by Steven Yi (2016.xx.xx)
--
-- ARGS
-- ain - signal to filter
-- acf/kcf - cutoff frequency 
-- ak/kk  - k-value that controls resonance, self-resonance occurs at k=17;
-- knlp - use non-linear processing: 
--        0 - none 
--        1 - normalized (outputs to range +-1.0)
--        2 - non-normalized (less expensive than normalized, range +-0.8)
-- ksaturation - saturation amount for non-linear processing 
--        (default: 1.0, greater values lead to higher saturation)
diodeLadder :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
diodeLadder ain xcf xk knlp ksaturation = fromGE $ do
    addUdoPlugin E.diodePlugin
    f <$> toGE ain <*> toGE xcf <*> toGE xk <*> toGE knlp <*> toGE ksaturation
    where f ain xcf xk knlp ksaturation = opcs "diode_ladder" [(Ar, [Ar, Xr, Xr, Kr, Kr])] [ain, xcf, xk, knlp, ksaturation]
