{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Booleans and logic, if-then-else, when, until
module Csound.Core.Types.Prim.Bool (
  BoolSig (..),
  unBoolSig,
  BoolD (..),
  unBoolD,
  boolSig,
  equals,
  notEquals,
  greater,
  greaterEquals,
  less,
  lessEquals,
) where

import Data.Boolean
import Prelude hiding ((<*))

import Csound.Core.Types.Prim.D
import Csound.Core.Types.Prim.Sig
import Csound.Core.Types.Prim.Spec
import Csound.Core.Types.Prim.Str
import Csound.Core.Types.Prim.Tab

import Csound.Core.State (Run)
import Csound.Core.Types.Prim.Val
import Csound.Dynamic (E, IfRate (..), Rate (..), ifExp)

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

  valRate = Kr

instance BoolVal BoolSig where
  boolValRate = IfKr

instance Val BoolD where
  fromE = BoolD

  toE x = case x of
    BoolD a -> a
    PrimBoolD b -> return $ if b then true else false

  valRate = Kr

instance BoolVal BoolD where
  boolValRate = IfIr

instance IsPrim BoolSig where
  type PrimOf BoolSig = Bool

  getPrim x = case x of
    PrimBoolSig a -> Just a
    _ -> Nothing

  fromPrim = PrimBoolSig

instance IsPrim BoolD where
  type PrimOf BoolD = Bool

  getPrim x = case x of
    PrimBoolD a -> Just a
    _ -> Nothing

  fromPrim = PrimBoolD

type instance BooleanOf Sig = BoolSig

type instance BooleanOf D = BoolD
type instance BooleanOf Str = BoolD
type instance BooleanOf Tab = BoolD
type instance BooleanOf Spec = BoolD

instance Boolean BoolSig where
  true = PrimBoolSig True
  false = PrimBoolSig False
  notB = liftPrim not notB
  (&&*) = liftPrim2 (&&) (&&*)
  (||*) = liftPrim2 (||) (||*)

instance Boolean BoolD where
  true = PrimBoolD True
  false = PrimBoolD False
  notB = liftPrim not notB
  (&&*) = liftPrim2 (&&) (&&*)
  (||*) = liftPrim2 (||) (||*)

------------------------------------------
-- if-then-else

instance IfB Sig where
  ifB x a b = case x of
    PrimBoolSig c -> if c then a else b
    _ -> liftE3 (ifExp IfKr) x a b

instance IfB D where
  ifB x a b = case x of
    PrimBoolD c -> if c then a else b
    _ -> liftE3 (ifExp IfIr) x a b

instance IfB Tab where
  ifB x a b = case x of
    PrimBoolD c -> if c then a else b
    _ -> liftE3 (ifExp IfIr) x a b

instance IfB Str where
  ifB x a b = case x of
    PrimBoolD c -> if c then a else b
    _ -> liftE3 (ifExp IfIr) x a b

instance IfB Spec where
  ifB x a b = case x of
    PrimBoolD c -> if c then a else b
    _ -> liftE3 (ifExp IfIr) x a b

instance EqB Sig where
  (==*) = liftPrim2 (==) (==*)
  (/=*) = liftPrim2 (/=) (/=*)

instance EqB D where
  (==*) = liftPrim2 (==) (==*)
  (/=*) = liftPrim2 (/=) (/=*)

instance OrdB Sig where (<*) = liftPrim2 (<) (<*); (>*) = liftPrim2 (>) (>*); (<=*) = liftPrim2 (<=) (<=*); (>=*) = liftPrim2 (>=) (>=*)
instance OrdB D where (<*) = liftPrim2 (<) (<*); (>*) = liftPrim2 (>) (>*); (<=*) = liftPrim2 (<=) (<=*); (>=*) = liftPrim2 (>=) (>=*)

-- | Creates a constant boolean signal.
boolSig :: BoolD -> BoolSig
boolSig x = case x of
  PrimBoolD b -> PrimBoolSig b
  BoolD a -> BoolSig a

infix 4 `equals`, `notEquals`, `less`, `lessEquals`, `greater`, `greaterEquals`

equals :: (EqB a) => a -> a -> BooleanOf a
equals = (==*)

notEquals :: (EqB a) => a -> a -> BooleanOf a
notEquals = (/=*)

less :: (OrdB a) => a -> a -> BooleanOf a
less = (<*)

greater :: (OrdB a) => a -> a -> BooleanOf a
greater = (>*)

lessEquals :: (OrdB a) => a -> a -> BooleanOf a
lessEquals = (<=*)

greaterEquals :: (OrdB a) => a -> a -> BooleanOf a
greaterEquals = (>=*)
