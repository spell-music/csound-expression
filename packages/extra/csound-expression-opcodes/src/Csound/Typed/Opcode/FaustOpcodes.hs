module Csound.Typed.Opcode.FaustOpcodes (
    
    
    
    faustctl) where

import Control.Monad.Trans.Class
import Control.Monad
import Csound.Dynamic
import Csound.Typed

-- 

-- | 
-- Adjusts a given control in a Faust DSP instance.
--
-- Faustctl will set a given control in a running faust program
--
-- >  faustctl  idsp,Scontrol,kval 
--
-- csound doc: <http://csound.com/docs/manual/faustctl.html>
faustctl ::  D -> Str -> Sig -> SE ()
faustctl b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unStr) b2 <*> (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep_ "faustctl" [(Xr,[Ir,Sr,Kr])] [a1,a2,a3]