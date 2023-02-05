-- | With ports we can communicate non-constant values between instruments
module Csound.Typed.Core.Types.SE.Port
  ( Port (..)
  , newPort
  ) where

import Csound.Typed.Core.Types.SE
import Csound.Typed.Core.Types.Tuple
import Csound.Typed.Core.Types.Prim.D
import Csound.Typed.Core.Types.Prim.Val

newtype Port a = Port { unPort :: D }
  deriving (IsPrim, Val, Tuple, Arg)

newPort :: Tuple a => a -> SE (Port a)
newPort = undefined

instance IsRef Port where
  readRef = undefined
  writeRef = undefined
