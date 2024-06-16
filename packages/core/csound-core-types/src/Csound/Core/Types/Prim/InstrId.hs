module Csound.Core.Types.Prim.InstrId
  ( ProcId (..)
  ) where

import Csound.Core.Types.Prim.Val

-- | Csound instrument name opf integer index
newtype ProcId ty args = ProcId { unProcId :: ty }
  deriving (Val, IsPrim)
