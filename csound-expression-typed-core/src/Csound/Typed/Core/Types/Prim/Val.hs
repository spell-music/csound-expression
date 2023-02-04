-- | Class for conversion of newtype-wrapped values
-- to low level dynamic representation
module Csound.Typed.Core.Types.Prim.Val
  ( Val (..)
  , IsPrim (..)
  , liftE
  , liftE2
  , liftE3
  , liftPrim
  , liftPrim2
  , liftPrim3
  ) where

import Csound.Dynamic (E)
import Csound.Typed.Core.State (Run)
import Data.Kind (Type)

class Val a where
  fromE :: Run E -> a
  toE   :: a -> Run E

liftE :: (Val a, Val b) => (E -> E) -> a -> b
liftE f a = fromE $ f <$> toE a

liftE2 :: (Val a, Val b, Val c) => (E -> E -> E) -> a -> b -> c
liftE2 f a b = fromE $ f <$> toE a <*> toE b

liftE3 :: (Val a, Val b, Val c, Val d) => (E -> E -> E -> E) -> a -> b -> c -> d
liftE3 f a b c = fromE $ f <$> toE a <*> toE b <*> toE c

class IsPrim a where
    type PrimOf a :: Type
    getPrim :: a -> Maybe (PrimOf a)
    fromPrim :: PrimOf a -> a

liftPrim :: (Val a, Val b, IsPrim a, IsPrim b) => (PrimOf a -> PrimOf b) -> (E -> E) -> (a -> b)
liftPrim primFun exprFun x = maybe (liftE exprFun x) (fromPrim . primFun) (getPrim x)

liftPrim2 :: (Val a, Val b, Val c, IsPrim a, IsPrim b, IsPrim c) => (PrimOf a -> PrimOf b -> PrimOf c) -> (E -> E -> E) -> (a -> b -> c)
liftPrim2 primFun exprFun xa xb = case getPrim xa of
  Just a -> case getPrim xb of
    Just b -> fromPrim $ primFun a b
    _ -> liftE2 exprFun xa xb
  _ -> liftE2 exprFun xa xb

liftPrim3 :: (Val a, Val b, Val c, Val d, IsPrim a, IsPrim b, IsPrim c, IsPrim d) => (PrimOf a -> PrimOf b -> PrimOf c -> PrimOf d) -> (E -> E -> E -> E) -> (a -> b -> c -> d)
liftPrim3 primFun exprFun xa xb xc = case getPrim xa of
  Just a -> case getPrim xb of
    Just b -> case getPrim xc of
      Just c -> fromPrim $ primFun a b c
      _ -> res
    _ -> res
  _ -> res
  where
    res = liftE3 exprFun xa xb xc
