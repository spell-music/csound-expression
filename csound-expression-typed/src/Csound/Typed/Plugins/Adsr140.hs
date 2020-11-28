module Csound.Typed.Plugins.Adsr140(  
	adsr140 
) where

import Data.Boolean
import Control.Monad.Trans.Class
import Control.Applicative

import Csound.Dynamic

import Csound.Typed.Types.Prim
import Csound.Typed.GlobalState
import qualified Csound.Typed.GlobalState.Elements as E(adsr140Plugin)


-------------------------------------------------------------------------------

-- | Gated, Re-triggerable ADSR modeled after the Doepfer A-140 
-- opcode adsr140, a, aakkkk
--
-- inputs: agate, aretrig, kattack, kdecay, ksustain, krelease 
adsr140 :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
adsr140 agate aretrig kattack kdecay ksustain krelease = fromGE $ do
    addUdoPlugin E.adsr140Plugin
    f <$> toGE agate <*> toGE aretrig <*> toGE kattack <*> toGE kdecay <*> toGE ksustain <*> toGE krelease
    where f agate aretrig kattack kdecay ksustain krelease = opcs "adsr140" [(Ar, [Ar, Ar, Kr, Kr, Kr, Kr])] [agate, aretrig, kattack, kdecay, ksustain, krelease]
