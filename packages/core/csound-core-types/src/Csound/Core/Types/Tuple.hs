{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language AllowAmbiguousTypes #-}
-- | Tuples of values
module Csound.Core.Types.Tuple
  ( FromTuple (..)
  , Tuple (..)
  , Arg
  , Sigs
  ) where

import Data.NumInstances.Tuple ()
import Data.Boolean

import Csound.Dynamic (E, Rate (..))
import Csound.Core.State (Run)
import Csound.Core.Types.Prim.Bool
import Csound.Core.Types.Prim.Sig
import Csound.Core.Types.Prim.D
import Csound.Core.Types.Prim.Tab
import Csound.Core.Types.Prim.Str
import Csound.Core.Types.Prim.Spec
import Csound.Core.Types.Prim.InstrId
import Csound.Core.Types.Prim.Val

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

class FromTuple a where
  fromTuple :: a -> Run [E]

class (FromTuple a) => Tuple a where
  toTuple :: Run [E] -> a
  tupleArity :: Int
  tupleRates :: [Rate]
  defTuple :: a

instance FromTuple () where
  fromTuple = const $ pure []

instance Tuple () where
  toTuple = const ()
  tupleArity = 0
  tupleRates = []
  defTuple = ()

instance FromTuple a => FromTuple [a] where
  fromTuple = fmap concat . mapM fromTuple

-- TODO: FromTuple instance for (Tuple a => Ref a)

instance FromTuple Sig where { fromTuple = fmap pure . toE }
instance Tuple Sig where { toTuple = fromE . fmap head; tupleRates = [valRate @Sig]; tupleArity = 1; defTuple = 0 }

instance FromTuple D where { fromTuple = fmap pure . toE }
instance Tuple D where { toTuple = fromE . fmap head; tupleRates = [valRate @D]; tupleArity = 1; defTuple = 0 }

instance FromTuple Tab where { fromTuple = fmap pure . toE }
instance Tuple Tab where { toTuple = fromE . fmap head; tupleRates = [valRate @Tab]; tupleArity = 1; defTuple = fromE $ pure (-1) }

instance FromTuple Str where { fromTuple = fmap pure . toE }
instance Tuple Str where { toTuple = fromE . fmap head; tupleRates = [valRate @Str]; tupleArity = 1; defTuple = "" }

instance FromTuple Spec where { fromTuple = fmap pure . toE }
instance Tuple Spec where { toTuple = fromE . fmap head; tupleRates = [valRate @Spec]; tupleArity = 1; defTuple = fromE (pure 0) }

instance FromTuple BoolSig where { fromTuple = fmap pure . toE }
instance Tuple BoolSig where { toTuple = fromE . fmap head; tupleRates = [valRate @BoolSig]; tupleArity = 1; defTuple = true }

instance FromTuple BoolD where { fromTuple = fmap pure . toE }
instance Tuple BoolD where { toTuple = fromE . fmap head; tupleRates = [valRate @BoolD]; tupleArity = 1; defTuple = true }

instance (Val ty, Arg a) => FromTuple (ProcId ty a) where { fromTuple = fmap pure . toE }
instance (Val ty, Arg a) => Tuple (ProcId ty a) where { toTuple = fromE . fmap head; tupleRates = [valRate @(ProcId ty a)]; tupleArity = 1; defTuple = fromE (pure 0) }

instance (FromTuple a, FromTuple b) => FromTuple (a, b) where
  fromTuple (a, b) = liftA2 (++) (fromTuple a) (fromTuple b)

instance (Tuple a, Tuple b) => Tuple (a, b) where
  tupleArity = tupleArity @a + tupleArity @b
  defTuple = (defTuple, defTuple)
  tupleRates = tupleRates @a ++ tupleRates @b
  toTuple = \es ->
    let
      arity = tupleArity @a
    in
      (toTuple $ take arity <$> es, toTuple $ drop arity <$> es)

instance (FromTuple a, FromTuple b, FromTuple c) => FromTuple (a, b, c) where
  fromTuple = fromTuple . split3

instance (Tuple a, Tuple b, Tuple c) => Tuple (a, b, c) where
  tupleArity = tupleArity @a + tupleArity @b + tupleArity @c
  tupleRates = tupleRates @a ++ tupleRates @b ++ tupleRates @c
  defTuple = (defTuple, defTuple, defTuple)
  toTuple = cons3 . toTuple

instance (FromTuple a, FromTuple b, FromTuple c, FromTuple d) => FromTuple (a, b, c, d) where
  fromTuple = fromTuple . split4

instance (Tuple a, Tuple b, Tuple c, Tuple d) => Tuple (a, b, c, d) where
  tupleArity = tupleArity @a + tupleArity @b + tupleArity @c + tupleArity @d
  tupleRates = tupleRates @a ++ tupleRates @b ++ tupleRates @c ++ tupleRates @d
  defTuple = (defTuple, defTuple, defTuple, defTuple)
  toTuple = cons4 . toTuple

instance (FromTuple a, FromTuple b, FromTuple c, FromTuple d, FromTuple e) => FromTuple (a, b, c, d, e) where
  fromTuple = fromTuple . split5

instance (Tuple a, Tuple b, Tuple c, Tuple d, Tuple e) => Tuple (a, b, c, d, e) where
  tupleArity = tupleArity @a + tupleArity @b + tupleArity @c + tupleArity @d + tupleArity @e
  tupleRates = tupleRates @a ++ tupleRates @b ++ tupleRates @c ++ tupleRates @d ++ tupleRates @e
  defTuple = (defTuple, defTuple, defTuple, defTuple, defTuple)
  toTuple = cons5 . toTuple

instance (FromTuple a, FromTuple b, FromTuple c, FromTuple d, FromTuple e, FromTuple f) => FromTuple (a, b, c, d, e, f) where
  fromTuple = fromTuple . split6

instance (Tuple a, Tuple b, Tuple c, Tuple d, Tuple e, Tuple f) => Tuple (a, b, c, d, e, f) where
  tupleArity = tupleArity @a + tupleArity @b + tupleArity @c + tupleArity @d + tupleArity @e + tupleArity @f
  tupleRates = tupleRates @a ++ tupleRates @b ++ tupleRates @c ++ tupleRates @d ++ tupleRates @e ++ tupleRates @f
  defTuple = (defTuple, defTuple, defTuple, defTuple, defTuple, defTuple)
  toTuple = cons6 . toTuple

instance (FromTuple a, FromTuple b, FromTuple c, FromTuple d, FromTuple e, FromTuple f, FromTuple g) => FromTuple (a, b, c, d, e, f, g) where
  fromTuple = fromTuple . split7

instance (Tuple a, Tuple b, Tuple c, Tuple d, Tuple e, Tuple f, Tuple g) => Tuple (a, b, c, d, e, f, g) where
  tupleArity = tupleArity @a + tupleArity @b + tupleArity @c + tupleArity @d + tupleArity @e + tupleArity @f + tupleArity @g
  tupleRates = tupleRates @a ++ tupleRates @b ++ tupleRates @c ++ tupleRates @d ++ tupleRates @e ++ tupleRates @f ++ tupleRates @g
  defTuple = (defTuple, defTuple, defTuple, defTuple, defTuple, defTuple, defTuple)
  toTuple = cons7 . toTuple

instance (FromTuple a, FromTuple b, FromTuple c, FromTuple d, FromTuple e, FromTuple f, FromTuple g, FromTuple g, FromTuple h) => FromTuple (a, b, c, d, e, f, g, h) where
  fromTuple = fromTuple . split8

instance (Tuple a, Tuple b, Tuple c, Tuple d, Tuple e, Tuple f, Tuple g, Tuple h) => Tuple (a, b, c, d, e, f, g, h) where
  tupleArity = tupleArity @a + tupleArity @b + tupleArity @c + tupleArity @d + tupleArity @e + tupleArity @f + tupleArity @g + tupleArity @h
  tupleRates = tupleRates @a ++ tupleRates @b ++ tupleRates @c ++ tupleRates @d ++ tupleRates @e ++ tupleRates @f ++ tupleRates @g ++ tupleRates @h
  defTuple = (defTuple, defTuple, defTuple, defTuple, defTuple, defTuple, defTuple, defTuple)
  toTuple = cons8 . toTuple

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
