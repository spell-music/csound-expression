module Csound.Typed.Plugins.Utilities (
  delay1k,
) where

import Csound.Dynamic

import Csound.Typed.GlobalState
import Csound.Typed.GlobalState.Elements qualified as E (delay1kPlugin)
import Csound.Typed.Types.Prim

-------------------------------------------------------------------------------

-- | Delay a control signal by single sample.
delay1k :: Sig -> Sig
delay1k ain = fromGE $ do
  addUdoPlugin E.delay1kPlugin
  f <$> toGE ain
  where
    f x = opcs "Delay1k" [(Kr, [Kr])] [x]
