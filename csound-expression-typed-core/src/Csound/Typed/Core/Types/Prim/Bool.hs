{-# OPTIONS_GHC -fno-warn-orphans #-}
-- Booleans and logic, if-then-else, when, until
module Csound.Typed.Core.Types.Prim.Bool
  ( BoolSig (..)
  , unBoolSig
  , BoolD (..)
  , unBoolD
  , boolSig
  , equalsTo
  , notEqualsTo
  , greaterThan
  , greaterThanEquals
  , lessThan
  , lessThanEquals
  ) where

import Prelude hiding ((<*))
import Data.Boolean

import Csound.Typed.Core.Types.Prim.Tab
import Csound.Typed.Core.Types.Prim.D
import Csound.Typed.Core.Types.Prim.Sig
import Csound.Typed.Core.Types.Prim.Spec
import Csound.Typed.Core.Types.Prim.Str

import Csound.Dynamic (E, IfRate (..), ifExp, Rate (..))
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

    valRate = Kr

instance Val BoolD   where
    fromE = BoolD

    toE x = case x of
        BoolD a -> a
        PrimBoolD b -> return $ if b then true else false

    valRate = Kr

instance IsPrim BoolSig where
    type PrimOf BoolSig = Bool

    getPrim x = case x of
        PrimBoolSig a -> Just a
        _         -> Nothing

    fromPrim = PrimBoolSig

instance IsPrim BoolD where
    type PrimOf BoolD = Bool

    getPrim x = case x of
        PrimBoolD a -> Just a
        _         -> Nothing

    fromPrim = PrimBoolD

type instance BooleanOf Sig  = BoolSig

type instance BooleanOf D    = BoolD
type instance BooleanOf Str  = BoolD
type instance BooleanOf Tab  = BoolD
type instance BooleanOf Spec = BoolD

instance Boolean BoolSig  where
  true = PrimBoolSig True
  false = PrimBoolSig False
  notB = liftPrim not notB
  (&&*) = liftPrim2 (&&) (&&*)
  (||*) = liftPrim2 (||) (||*)

instance Boolean BoolD    where
  true = PrimBoolD   True
  false = PrimBoolD   False
  notB = liftPrim   not notB
  (&&*) = liftPrim2   (&&) (&&*)
  (||*) = liftPrim2   (||) (||*)

------------------------------------------
-- if-then-else

instance IfB Sig  where
  ifB x a b = case x of
    PrimBoolSig c -> if c then a else b
    _             -> liftE3 (ifExp IfKr) x a b

instance IfB D    where
  ifB x a b = case x of
    PrimBoolD c -> if c then a else b
    _           -> liftE3 (ifExp IfIr) x a b

instance IfB Tab  where
  ifB x a b = case x of
    PrimBoolD c -> if c then a else b
    _           -> liftE3 (ifExp IfIr) x a b

instance IfB Str  where
  ifB x a b = case x of
    PrimBoolD c -> if c then a else b
    _           -> liftE3 (ifExp IfIr) x a b

instance IfB Spec where
  ifB x a b = case x of
    PrimBoolD c -> if c then a else b
    _           -> liftE3 (ifExp IfIr) x a b

instance EqB Sig  where
  (==*) = liftPrim2 (==) (==*)
  (/=*) = liftPrim2 (/=) (/=*)

instance EqB D where
  (==*) = liftPrim2 (==) (==*)
  (/=*) = liftPrim2 (/=) (/=*)

instance OrdB Sig where { (<*)  = liftPrim2 (<) (<*) ;    (>*)  = liftPrim2 (>) (>*);     (<=*) = liftPrim2 (<=) (<=*);    (>=*) = liftPrim2 (>=) (>=*) }
instance OrdB D   where { (<*)  = liftPrim2 (<) (<*) ;    (>*)  = liftPrim2 (>) (>*);     (<=*) = liftPrim2 (<=) (<=*);    (>=*) = liftPrim2 (>=) (>=*) }

-- | Creates a constant boolean signal.
boolSig :: BoolD -> BoolSig
boolSig x = case x of
    PrimBoolD b -> PrimBoolSig b
    BoolD a     -> BoolSig a

infix  4  `equalsTo`, `notEqualsTo`, `lessThan`, `lessThanEquals`, `greaterThanEquals`, `greaterThan`

equalsTo :: EqB a => a -> a -> BooleanOf a
equalsTo = (==*)

notEqualsTo :: EqB a => a -> a -> BooleanOf a
notEqualsTo = (/=*)

lessThan :: OrdB a => a -> a -> BooleanOf a
lessThan = (<*)

greaterThan :: OrdB a => a -> a -> BooleanOf a
greaterThan = (>*)

lessThanEquals :: OrdB a => a -> a -> BooleanOf a
lessThanEquals = (<=*)

greaterThanEquals :: OrdB a => a -> a -> BooleanOf a
greaterThanEquals = (>=*)
