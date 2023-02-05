module Csound.Typed.Core.Types.Prim.InstrId
  ( ProcId (..)
  ) where

import Csound.Typed.Core.Types.Prim.Val

newtype ProcId ty args = ProcId { unProcId :: ty }
  deriving (Val, IsPrim)
