module Csound.Typed.Plugins.Utilities(
  delay1k
) where

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
    where f x = opcs "Delay1k" [(Kr, [Kr])] [x]
