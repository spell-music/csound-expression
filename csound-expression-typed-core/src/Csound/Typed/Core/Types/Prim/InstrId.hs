module Csound.Typed.Core.Types.Prim.InstrId
  ( InstrId (..)
  , getInstrIdRate
  ) where

import Csound.Dynamic (Rate (..), E)
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.State (Run)
import Csound.Typed.Core.Types.Prim.Val

-- | Typed instrument identificator.
-- The type is a type of the argument that triggers an instrument
data InstrId a
  = PrimInstrId Dynamic.InstrId
  | InstrId { unInstrId :: Run E }

instance Val (InstrId a) where
  fromE = InstrId

  toE   = \case
    PrimInstrId ix -> pure (Dynamic.prim (Dynamic.PrimInstrId ix))
    InstrId ix -> ix

getInstrIdRate :: InstrId a -> Rate
getInstrIdRate = \case
  PrimInstrId ix -> Dynamic.instrIdRate ix
  _              -> Ir
