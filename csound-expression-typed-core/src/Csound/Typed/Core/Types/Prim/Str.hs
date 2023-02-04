module Csound.Typed.Core.Types.Prim.Str
  ( Str (..)
  ) where

import Data.String

import Csound.Dynamic (E)
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.State (Run)
import Csound.Typed.Core.Types.Prim.Val

-- | Strings
newtype Str = Str { unStr :: Run E }

instance Val Str where
  fromE = Str
  toE   = unStr

instance IsString Str where
  fromString = Str . pure . Dynamic.prim . Dynamic.PrimString . fromString
