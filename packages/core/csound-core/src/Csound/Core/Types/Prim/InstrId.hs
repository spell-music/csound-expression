module Csound.Core.Types.Prim.InstrId
  ( ProcId (..)
  ) where

import Csound.Core.Types.Prim.Val

newtype ProcId ty args = ProcId { unProcId :: ty }
  deriving (Val, IsPrim)
