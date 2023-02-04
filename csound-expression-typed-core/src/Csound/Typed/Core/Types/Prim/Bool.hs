module Csound.Typed.Core.Types.Prim.Bool
  ( BoolSig (..)
  , unBoolSig
  , BoolD (..)
  , unBoolD
  ) where

import Data.Boolean

import Csound.Typed.Core.Types.Prim.Tab
import Csound.Typed.Core.Types.Prim.D
import Csound.Typed.Core.Types.Prim.Sig
import Csound.Typed.Core.Types.Prim.Str

import Csound.Dynamic (E)
import Csound.Typed.Core.State (Run)
import Csound.Typed.Core.Types.Prim.Val

-- | A signal of booleans.
data BoolSig
    = BoolSig (Run E)
    | PrimBoolSig Bool

unBoolSig :: BoolSig -> Run E
unBoolSig = toE

-- | A constant boolean value.
data BoolD
    = BoolD (Run E)
    | PrimBoolD Bool

unBoolD :: BoolD -> Run E
unBoolD = toE

instance Val BoolSig where
    fromE = BoolSig
    toE x = case x of
        BoolSig a -> a
        PrimBoolSig b -> return $ if b then true else false

instance Val BoolD   where
    fromE = BoolD
    toE x = case x of
        BoolD a -> a
        PrimBoolD b -> return $ if b then true else false

type instance BooleanOf Sig  = BoolSig

type instance BooleanOf D    = BoolD
type instance BooleanOf Str  = BoolD
type instance BooleanOf Tab  = BoolD
-- type instance BooleanOf Spec = BoolD



