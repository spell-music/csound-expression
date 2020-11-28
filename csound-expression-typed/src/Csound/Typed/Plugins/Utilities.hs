module Csound.Typed.Plugins.Utilities(  
	delay1k 
) where

import Data.Boolean
import Control.Monad.Trans.Class
import Control.Applicative

import Csound.Dynamic

import Csound.Typed.Types.Prim
import Csound.Typed.GlobalState
import qualified Csound.Typed.GlobalState.Elements as E(delay1kPlugin)


-------------------------------------------------------------------------------

-- | Delay a control signal by single sample.
delay1k :: Sig -> Sig
delay1k ain = fromGE $ do
    addUdoPlugin E.delay1kPlugin
    f <$> toGE ain
    where f ain = opcs "Delay1k" [(Kr, [Kr])] [ain]
