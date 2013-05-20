-- | instrument p-arguments
module Csound.Exp.Arg(        
    Arg(..), arg, toNote, arity, 
    ArgMethods, toArg, makeArgMethods,
) where

import Control.Applicative

import Csound.Exp
import Csound.Exp.Wrapper(Val, toExp, D, Str, p)

------------------------------------------------
-- basic extractor

getPrimUnsafe :: Val a => a -> Prim
getPrimUnsafe x = case toExp x of
    ExpPrim a -> a
    _ -> error "Arg.hs:getPrimUnsafe - value is not prim"

-- | The abstract type of methods for the class 'Arg'.
data ArgMethods a = ArgMethods 
    { arg_    :: Int -> a
    , toNote_ :: a -> [Prim]
    , arity_  :: a -> Int }

arg :: Arg a => Int -> a
arg = arg_ argMethods

toNote :: Arg a => a -> [Prim]
toNote = toNote_ argMethods

arity :: Arg a => a -> Int
arity = arity_ argMethods

toArg :: Arg a => a
toArg = arg 4

-- | Defines instance of type class 'Arg' for a new type in terms of an already defined one.
makeArgMethods :: (Arg a) => (a -> b) -> (b -> a) -> ArgMethods b
makeArgMethods to from = ArgMethods {
    arg_ = to . arg,
    toNote_ = toNote . from,
    arity_ = const $ arity $ proxy to }
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
        { arg_ = const ()
        , toNote_ = const []
        , arity_ = const 0 }

instance Arg InstrId where
    argMethods = ArgMethods 
        { arg_ = error "method arg is undefined for InstrId"
        , toNote_ = pure . PrimInstrId
        , arity_ = const 0 }

instance Arg D where
    argMethods = ArgMethods {
        arg_ = p,
        toNote_ = pure . getPrimUnsafe,
        arity_ = const 1 }

instance Arg Str where
    argMethods = ArgMethods {
        arg_ = p,
        toNote_ = pure . getPrimUnsafe,
        arity_ = const 1 }

instance Arg Tab where
    argMethods = ArgMethods {
        arg_ = p,
        toNote_ = pure . getPrimUnsafe,
        arity_ = const 1 }

instance (Arg a, Arg b) => Arg (a, b) where
    argMethods = ArgMethods arg' toNote' arity' 
        where arg' n = (a, b)
                  where a = arg n
                        b = arg (n + arity a)
              toNote' (a, b) = toNote a ++ toNote b
              arity' x = let (a, b) = proxy x in arity a + arity b    
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

