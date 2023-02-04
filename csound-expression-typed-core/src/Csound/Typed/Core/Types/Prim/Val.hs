-- | Class for conversion of newtype-wrapped values
-- to low level dynamic representation
module Csound.Typed.Core.Types.Prim.Val
  ( Val (..)
  , liftE
  , liftE2
  ) where

import Csound.Dynamic (E)
import Csound.Typed.Core.State (Run)

class Val a where
  fromE :: Run E -> a
  toE   :: a -> Run E

liftE :: (Val a, Val b) => (E -> E) -> a -> b
liftE f a = fromE $ f <$> toE a

liftE2 :: (Val a, Val b, Val c) => (E -> E -> E) -> a -> b -> c
liftE2 f a b = fromE $ f <$> toE a <*> toE b
