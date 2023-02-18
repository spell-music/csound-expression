module Csound.Core.Types.Prim.Str
  ( Str (..)
  ) where

import Data.String

import Csound.Dynamic (E, Rate(..))
import Csound.Dynamic qualified as Dynamic
import Csound.Core.State (Run)
import Csound.Core.Types.Prim.Val

-- | Strings
newtype Str = Str { unStr :: Run E }

instance Val Str where
  fromE = Str
  toE   = unStr
  valRate = Sr

instance IsString Str where
  fromString = Str . pure . Dynamic.prim . Dynamic.PrimString . fromString
