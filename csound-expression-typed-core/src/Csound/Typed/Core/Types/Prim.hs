module Csound.Typed.Core.Types.Prim
  ( Sig (..)
  , D (..)
  , Str (..)
  , InstrId (..)
  , module X
  ) where


import Data.String

import Csound.Typed.Core.State
import Csound.Dynamic (E)
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.Types.Prim.D as X
import Csound.Typed.Core.Types.Prim.Sig as X
import Csound.Typed.Core.Types.Prim.Tab as X
import Csound.Typed.Core.Types.Prim.Val as X

newtype Str = Str { unStr :: Run E }

newtype InstrId = InstrId { unInstrId :: Run E }

instance Val Str where
  fromE = Str
  toE   = unStr

instance Val InstrId where
  fromE = InstrId
  toE   = unInstrId

instance IsString Str where
  fromString = fromE . pure . Dynamic.prim . Dynamic.PrimString . fromString
