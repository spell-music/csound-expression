module Csound.Typed.Plugins.Audaciouseq( 
    audaciousEq   
) where

import Data.Boolean
import Control.Monad.Trans.Class
import Control.Applicative

import Csound.Dynamic

import Csound.Typed.Types
import Csound.Typed.GlobalState
import qualified Csound.Typed.GlobalState.Elements as E(audaciouseqPlugin)

-------------------------------------------------------------------------------

-- | opcode audaciouseq, a, kkkkkkkkkka
--
-- inputs: kgain1, kgain2, kgain3, kgain4, kgain5, 
--     kgain6, kgain7, kgain8, kgain9, kgain10 ain
--
-- 10-band EQ 
--   Input: kgain1, kgain2, ... kgain10, asig
--   Output: aout
--
--   10 kgain arguments maps to each band
--   Bands are: 31.25, 52.6, 125, 500, 1000, 
--              2000, 4000, 8000, 16000 
--
audaciousEq :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
audaciousEq kgain1 kgain2 kgain3 kgain4 kgain5 kgain6 kgain7 kgain8 kgain9 kgain10 ain = fromGE $ do
    addUdoPlugin E.audaciouseqPlugin
    f <$> toGE ain <*> toGE kgain1 <*> toGE kgain2 <*> toGE kgain3 <*> toGE kgain4 <*> toGE kgain5 <*> toGE kgain6 <*> toGE kgain7 <*> toGE kgain8 <*> toGE kgain9 <*> toGE kgain10
    where f ain kgain1 kgain2 kgain3 kgain4 kgain5 kgain6 kgain7 kgain8 kgain9 kgain10 = opcs "audaciouseq" [(Ar, [Ar, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [ain, kgain1, kgain2, kgain3, kgain4, kgain5, kgain6, kgain7, kgain8, kgain9, kgain10]
