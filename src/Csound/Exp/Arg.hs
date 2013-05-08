-- | instrument p-arguments
module Csound.Exp.Arg(        
    Arg(..), ArgMethods(..), toArg, makeArgMethods,
) where

import Control.Applicative

import Csound.Exp
import Csound.Exp.Wrapper(Val, toExp, D, Str, p)

------------------------------------------------
-- basic extractor

getPrimUnsafe :: Val a => a -> Prim
getPrimUnsafe a = case toExp a of
    ExpPrim p -> p

-- | The abstract type of methods for the class 'Arg'.
data ArgMethods a = ArgMethods 
    { arg :: Int -> a
    , toNote :: a -> [Prim]
    , arity :: a -> Int
    }

toArg :: Arg a => a
toArg = arg argMethods 4

-- | Defines instance of type class 'Arg' for a new type in terms of an already defined one.
makeArgMethods :: (Arg a) => (a -> b) -> (b -> a) -> ArgMethods b
makeArgMethods to from = ArgMethods {
    arg = to . arg argMethods,
    toNote = toNote argMethods . from,
    arity = const $ arity argMethods $ proxy to }
    where proxy :: (a -> b) -> a
          proxy = undefined

-- | Describes all Csound values that can be used in the score section. 
-- Instruments are triggered with the values from this type class.
-- Actual methods are hidden, but you can easily make instances for your own types
-- with function 'makeArgMethods'. You need to describe the new instance in  terms 
-- of some existing one. For example:
--
-- > data Note = Note 
-- >     { noteAmplitude    :: D
-- >     , notePitch        :: D
-- >     , noteVibrato      :: D
-- >     , noteSample       :: S
-- >     }
-- > 
-- > instance Arg Note where
-- >     argMethods = makeArgMethods to from
-- >         where to (amp, pch, vibr, sample) = Note amp pch vibr sample
-- >               from (Note amp pch vibr sample) = (amp, pch, vibr, sample)
-- 
-- Then you can use this type in an instrument definition.
-- 
-- > instr :: Note -> Out
-- > instr x = ...

class Arg a where
    argMethods :: ArgMethods a

instance Arg () where
    argMethods = ArgMethods 
        { arg = const ()
        , toNote = const []
        , arity = const 0 }

instance Arg D where
    argMethods = ArgMethods {
        arg = p,
        toNote = pure . getPrimUnsafe,
        arity = const 1 }

instance Arg Str where
    argMethods = ArgMethods {
        arg = p,
        toNote = pure . getPrimUnsafe,
        arity = const 1 }

instance Arg Tab where
    argMethods = ArgMethods {
        arg = p,
        toNote = pure . getPrimUnsafe,
        arity = const 1 }

instance (Arg a, Arg b) => Arg (a, b) where
    argMethods = ArgMethods arg' toNote' arity' 
        where arg' n = (a, b)
                  where a = arg argMethods n
                        b = arg argMethods (n + arity argMethods a)
              toNote' (a, b) = toNote argMethods a ++ toNote argMethods b
              arity' x = let (a, b) = proxy x in arity argMethods a + arity argMethods b    
                  where proxy :: (a, b) -> (a, b)
                        proxy = const (undefined, undefined)

instance (Arg a, Arg b, Arg c) => Arg (a, b, c) where
    argMethods = makeArgMethods to from
        where to (a, (b, c)) = (a, b, c)
              from (a, b, c) = (a, (b, c))

instance (Arg a, Arg b, Arg c, Arg d) => Arg (a, b, c, d) where
    argMethods = makeArgMethods to from
        where to (a, (b, c, d)) = (a, b, c, d)
              from (a, b, c, d) = (a, (b, c, d))

instance (Arg a, Arg b, Arg c, Arg d, Arg e) => Arg (a, b, c, d, e) where
    argMethods = makeArgMethods to from
        where to (a, (b, c, d, e)) = (a, b, c, d, e)
              from (a, b, c, d, e) = (a, (b, c, d, e))

instance (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f) => Arg (a, b, c, d, e, f) where
    argMethods = makeArgMethods to from
        where to (a, (b, c, d, e, f)) = (a, b, c, d, e, f)
              from (a, b, c, d, e, f) = (a, (b, c, d, e, f))

instance (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f, Arg g) => Arg (a, b, c, d, e, f, g) where
    argMethods = makeArgMethods to from
        where to (a, (b, c, d, e, f, g)) = (a, b, c, d, e, f, g)
              from (a, b, c, d, e, f, g) = (a, (b, c, d, e, f, g))


instance (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f, Arg g, Arg h) => Arg (a, b, c, d, e, f, g, h) where
    argMethods = makeArgMethods to from
        where to (a, (b, c, d, e, f, g, h)) = (a, b, c, d, e, f, g, h)
              from (a, b, c, d, e, f, g, h) = (a, (b, c, d, e, f, g, h))

