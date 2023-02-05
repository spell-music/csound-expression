{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language AllowAmbiguousTypes #-}
-- | Tuples of values
module Csound.Typed.Core.Types.Tuple
  ( Tuple (..), Arg, Sigs
  , fromTuple
  , toTuple
  , defTuple
  , tupleRates
  , tupleArity
  , makeTupleMethods
  , TupleMethods (..)
  ) where

import Data.NumInstances.Tuple ()

import Control.Applicative (liftA2)

import Csound.Dynamic (E, Rate (..))
import Csound.Typed.Core.State (Run)
import Csound.Typed.Core.Types.Prim

class Tuple a => Arg a where

instance Arg ()
instance Arg D
instance Arg Str
instance Arg Tab
instance Arg a => Arg (ProcId D a)
instance Arg a => Arg (ProcId Str a)

instance (Arg a, Arg b) => Arg (a, b)
instance (Arg a, Arg b, Arg c) => Arg (a, b, c)
instance (Arg a, Arg b, Arg c, Arg d) => Arg (a, b, c, d)
instance (Arg a, Arg b, Arg c, Arg d, Arg e) => Arg (a, b, c, d, e)
instance (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f) => Arg (a, b, c, d, e, f)
instance (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f, Arg h) => Arg (a, b, c, d, e, f, h)
instance (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f, Arg h, Arg g) => Arg (a, b, c, d, e, f, h, g)

class (Num a, Tuple a) => Sigs a where

instance Sigs Sig

instance (Sigs a1, Sigs a2) => Sigs (a1, a2)
instance (Sigs a1, Sigs a2, Sigs a3) => Sigs (a1, a2, a3)
instance (Sigs a1, Sigs a2, Sigs a3, Sigs a4) => Sigs (a1, a2, a3, a4)
instance (Sigs a1, Sigs a2, Sigs a3, Sigs a4, Sigs a5) => Sigs (a1, a2, a3, a4, a5)
instance (Sigs a1, Sigs a2, Sigs a3, Sigs a4, Sigs a5, Sigs a6) => Sigs (a1, a2, a3, a4, a5, a6)
instance (Sigs a1, Sigs a2, Sigs a3, Sigs a4, Sigs a5, Sigs a6, Sigs a7) => Sigs (a1, a2, a3, a4, a5, a6, a7)
instance (Sigs a1, Sigs a2, Sigs a3, Sigs a4, Sigs a5, Sigs a6, Sigs a7, Sigs a8) => Sigs (a1, a2, a3, a4, a5, a6, a7, a8)

-- | A tuple of Csound values.
class Tuple a where
    tupleMethods :: TupleMethods a

data TupleMethods a = TupleMethods
  { fromTuple_  :: a -> Run [E]
  , toTuple_    :: Run [E] -> a
  , tupleRates_ :: [Rate]
  , tupleArity_ :: Int
  , defTuple_   :: a
  }

-- | Defines instance of type class 'Tuple' for a new type in terms of an already defined one.
makeTupleMethods :: forall a b . (Tuple a) => (a -> b) -> (b -> a) -> TupleMethods b
makeTupleMethods to from = TupleMethods
    { fromTuple_  = fromTuple . from
    , toTuple_    = to . toTuple
    , tupleRates_ = tupleRates @a
    , tupleArity_ = tupleArity @a
    , defTuple_   = to defTuple }

fromTuple :: Tuple a => a -> Run [E]
fromTuple = fromTuple_ tupleMethods

toTuple :: Tuple a => Run [E] -> a
toTuple = toTuple_ tupleMethods

tupleRates :: forall a . Tuple a => [Rate]
tupleRates = tupleRates_ @a tupleMethods

tupleArity :: forall a . Tuple a => Int
tupleArity = tupleArity_ @a tupleMethods

defTuple :: Tuple a => a
defTuple = defTuple_ tupleMethods

primTuple :: forall a . Val a => TupleMethods a
primTuple = TupleMethods
  { fromTuple_ = fmap pure . toE
  , toTuple_ = fromE . fmap head
  , tupleArity_ = 1
  , tupleRates_ = [valRate @a]
  , defTuple_ = fromE (pure 0)
  }

instance Tuple () where
  tupleMethods = TupleMethods
    { tupleArity_ = 0
    , tupleRates_ = []
    , defTuple_ = ()
    , fromTuple_ = const $ pure []
    , toTuple_ = const ()
    }

instance Tuple Sig where { tupleMethods = primTuple }
instance Tuple D   where { tupleMethods = primTuple }
instance Tuple Tab where { tupleMethods = primTuple }
instance Tuple Str where { tupleMethods = primTuple }
instance Tuple Spec where { tupleMethods = primTuple }
instance (Val ty, Arg a) => Tuple (ProcId ty a) where { tupleMethods = primTuple }

instance (Tuple a, Tuple b) => Tuple (a, b) where
  tupleMethods = TupleMethods
    { tupleArity_ = tupleArity @a + tupleArity @b
    , defTuple_ = (defTuple, defTuple)
    , tupleRates_ = tupleRates @a ++ tupleRates @b
    , fromTuple_ = \(a, b) -> liftA2 (++) (fromTuple a) (fromTuple b)
    , toTuple_ = \es ->
        let arity = tupleArity @a
        in  (toTuple $ take arity <$> es, toTuple $ drop arity <$> es)
    }

instance (Tuple a, Tuple b, Tuple c) => Tuple (a, b, c) where tupleMethods = makeTupleMethods cons3 split3
instance (Tuple a, Tuple b, Tuple c, Tuple d) => Tuple (a, b, c, d) where tupleMethods = makeTupleMethods cons4 split4
instance (Tuple a, Tuple b, Tuple c, Tuple d, Tuple e) => Tuple (a, b, c, d, e) where tupleMethods = makeTupleMethods cons5 split5
instance (Tuple a, Tuple b, Tuple c, Tuple d, Tuple e, Tuple f) => Tuple (a, b, c, d, e, f) where tupleMethods = makeTupleMethods cons6 split6
instance (Tuple a, Tuple b, Tuple c, Tuple d, Tuple e, Tuple f, Tuple g) => Tuple (a, b, c, d, e, f, g) where tupleMethods = makeTupleMethods cons7 split7
instance (Tuple a, Tuple b, Tuple c, Tuple d, Tuple e, Tuple f, Tuple g, Tuple h) => Tuple (a, b, c, d, e, f, g, h) where tupleMethods = makeTupleMethods cons8 split8

-----------------------------------------------------------------------------------
-- boiler plate tuple helpers

cons3 :: (a, (b, c)) -> (a, b, c)
cons4 :: (a, (b, c, d)) -> (a, b, c, d)
cons5 :: (a, (b, c, d, e)) -> (a, b, c, d, e)
cons6 :: (a, (b, c, d, e, f)) -> (a, b, c, d, e, f)
cons7 :: (a, (b, c, d, e, f, g)) -> (a, b, c, d, e, f, g)
cons8 :: (a, (b, c, d, e, f, g, h)) -> (a, b, c, d, e, f, g, h)

cons3 (a, (b, c)) = (a, b, c)
cons4 (a, (b, c, d)) = (a, b, c, d)
cons5 (a, (b, c, d, e)) = (a, b, c, d, e)
cons6 (a, (b, c, d, e, f)) = (a, b, c, d, e, f)
cons7 (a, (b, c, d, e, f, g)) = (a, b, c, d, e, f, g)
cons8 (a, (b, c, d, e, f, g, h)) = (a, b, c, d, e, f, g, h)

split3 :: (a, b, c) -> (a, (b, c))
split4 :: (a, b, c, d) -> (a, (b, c, d))
split5 :: (a, b, c, d, e) -> (a, (b, c, d, e))
split6 :: (a, b, c, d, e, f) -> (a, (b, c, d, e, f))
split7 :: (a, b, c, d, e, f, g) -> (a, (b, c, d, e, f, g))
split8 :: (a, b, c, d, e, f, g, h) -> (a, (b, c, d, e, f, g, h))

split3 (a, b, c) = (a, (b, c))
split4 (a, b, c, d) = (a, (b, c, d))
split5 (a, b, c, d, e) = (a, (b, c, d, e))
split6 (a, b, c, d, e, f) = (a, (b, c, d, e, f))
split7 (a, b, c, d, e, f, g) = (a, (b, c, d, e, f, g))
split8 (a, b, c, d, e, f, g, h) = (a, (b, c, d, e, f, g, h))


------------------------------------------------------------------------------------------
-- missing num instance

lift8 :: (a->u) -> (b->v) -> (c->w) -> (d->x) -> (e->y) -> (f->t) -> (g->z) -> (h->z1)
      -> (a,b,c,d,e,f,g,h) -> (u,v,w,x,y,t,z,z1)
lift8 fa fb fc fd fe ff fg fh (a,b,c,d,e,f,g,h) =
  (fa a, fb b, fc c, fd d, fe e, ff f, fg g, fh h)

instance (Num a, Num b, Num c, Num d, Num e, Num f, Num g, Num h) => Num (a,b,c,d,e,f,g,h) where
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n)
  (a,b,c,d,e,f,g,h) + (a',b',c',d',e',f',g',h') = (a+a',b+b',c+c',d+d',e+e',f+f',g+g',h+h')
  (a,b,c,d,e,f,g,h) - (a',b',c',d',e',f',g',h') = (a-a',b-b',c-c',d-d',e-e',f-f',g-g',h-h')
  (a,b,c,d,e,f,g,h) * (a',b',c',d',e',f',g',h') = (a*a',b*b',c*c',d*d',e*e',f*f',g*g', h*h')
  negate = lift8 negate negate negate negate negate negate negate negate
  abs    = lift8 abs abs abs abs abs abs abs abs
  signum = lift8 signum signum signum signum signum signum signum signum

