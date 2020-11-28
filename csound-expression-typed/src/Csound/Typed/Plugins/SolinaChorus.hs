module Csound.Typed.Plugins.SolinaChorus(  
    solinaChorus, testSolinaChorus    
) where

import Data.Boolean
import Control.Monad.Trans.Class
import Control.Applicative

import Csound.Dynamic

import Csound.Typed.Types
import Csound.Typed.GlobalState
import qualified Csound.Typed.GlobalState.Elements as E(solinaChorusPlugin)

-- Solina Chorus, based on Solina String Ensemble Chorus Module
--  
--   based on:
--
--   J. Haible: Triple Chorus
--   http://jhaible.com/legacy/triple_chorus/triple_chorus.html
-- 
-- > solinaChorus (lfo_amp1, lfo_freq1) (lfo_amp2, lfo_freq2)
--
--   Author: Steven Yi
--   Date: 2016.05.22  
--
-- Example
--
-- > x = solinaChorus (0.6, 0.18) (0.2, 6) x
solinaChorus :: (Sig, Sig) -> (Sig, Sig) -> Sig -> Sig
solinaChorus (amp1, cps1) (amp2, cps2) ain = solina_chorus ain cps1 amp1 cps2 amp2

testSolinaChorus :: Sig -> Sig
testSolinaChorus x = solinaChorus (0.6, 0.18) (0.2, 6) x

-------------------------------------------------------------------------------

--   Solina Chorus, based on Solina String Ensemble Chorus Module
--  
--   based on:
--
--   J. Haible: Triple Chorus
--   http://jhaible.com/legacy/triple_chorus/triple_chorus.html
--
--   Hugo Portillo: Solina-V String Ensemble
--   http://www.native-instruments.com/en/reaktor-community/reaktor-user-library/entry/show/4525/ 
--
--   Parabola tabled shape borrowed from Iain McCurdy delayStereoChorus.csd:
--   http://iainmccurdy.org/CsoundRealtimeExamples/Delays/delayStereoChorus.csd
--
--   Author: Steven Yi
--   Date: 2016.05.22  
--
--  opcode solina_chorus, a, aKKKK
--
--  aLeft, klfo_freq1, klfo_amp1, klfo_freq2, klfo_amp2 xin
solina_chorus :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
solina_chorus aLeft klfo_freq1 klfo_amp1 klfo_freq2 klfo_amp2 = fromGE $ do
    addUdoPlugin E.solinaChorusPlugin
    f <$> toGE aLeft <*> toGE klfo_freq1 <*> toGE klfo_amp1 <*> toGE klfo_freq2 <*> toGE klfo_amp2
    where f aLeft klfo_freq1 klfo_amp1 klfo_freq2 klfo_amp2 = opcs "solina_chorus" [(Ar, [Ar, Kr, Kr, Kr, Kr])] [aLeft, klfo_freq1, klfo_amp1, klfo_freq2, klfo_amp2]
