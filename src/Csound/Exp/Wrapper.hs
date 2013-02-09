{-# Language 
        TypeSynonymInstances,
        FlexibleInstances #-}
module Csound.Exp.Wrapper(
    Out, Sig, I, D, Str, BoolSig(..), Spec, ToSig(..),
    SE, se, se_, runSE, execSE,
    Arg(..), ArgMethods(..), toArg, makeArgMethods,
    CsdTuple(..), multiOuts,
    Val(..),
    str, double, int, ar, kr, ir,
    tfm, pref, prim, p,
    isMultiOutSignature,
    noRate, setRate, getRates,
    readVar, writeVar, gOutVar,
    Channel
) where

import Control.Applicative
import Control.Monad(ap)
import Control.Monad.Trans.State

import Data.String
import Data.Fix
import Control.Monad.Trans.State

import Csound.Exp

type Channel = Int

-- | Output of the instrument.
type Out = SE [Sig]


-- | Audio or control rate signals. 
newtype Sig = Sig { unSig :: E }

-- | Integers.
newtype I = I { unI :: E }

-- | Doubles.
newtype D = D { unD :: E }

-- | Strings.
newtype Str = Str { unStr :: E }

-- | Boolean signals. Use functions from the module "Data.Boolean" to make boolean signals
-- out of simple signals.
newtype BoolSig = BoolSig { unBoolSig :: E }

-- | Spectrum of the signal (see "FFT and Spectral Processing" in the "Csound.Opcode.Advanced"). 
newtype Spec = Spec { unSpec :: E }

------------------------------------------------
-- side effects

-- | Csound's synonym for 'IO'-monad. 'SE' means Side Effect. 
-- You will bump into 'SE' trying to read and write to delay lines,
-- making random signals or trying to save your audio to file. 
-- Instrument is expected to return a value of @SE [Sig]@. 
-- So it's okay to do some side effects when playing a note.
newtype SE a = SE { unSE :: State E a }

instance Functor SE where
    fmap f = SE . fmap f . unSE

instance Applicative SE where
    pure = return
    (<*>) = ap

instance Monad SE where
    return = SE . return
    ma >>= mf = SE $ unSE ma >>= unSE . mf

runSE :: SE a -> (a, E)
runSE a = runState (unSE a) (unD (p 3 :: D))

execSE :: SE a -> E
execSE = snd . runSE

------------------------------------------------
-- basic constructors
  
noRate :: Val a => Exp E -> a
noRate = ratedExp Nothing
  
withRate :: Val a => Rate -> Exp E -> a
withRate r = ratedExp (Just r)

ratedExp :: Val a => Maybe Rate -> Exp E -> a
ratedExp r = wrap . RatedExp r Nothing

prim :: Val a => Prim -> a
prim = wrap . noRate . ExpPrim 

pref :: Name -> Signature -> Info
pref name signature = Info name signature Prefix Nothing

inf :: Name -> Signature -> Info
inf name signature = Info name signature Infix Nothing
  
tfm :: Val a => Info -> [RatedExp E] -> a
tfm info args = wrap $ noRate $ Tfm info $ map Fix args

gvar, var :: Val a => Rate -> Name -> a

var  = mkVar LocalVar 
gvar = mkVar GlobalVar

mkVar :: Val a => VarType -> Rate -> String -> a
mkVar ty rate name = wrap $ noRate $ ReadVar (Var ty rate name)

p :: Val a => Int -> a
p = prim . P

-- | Converts Haskell's integers to Csound's integers
int :: Int -> I
int = prim . PrimInt

-- | Converts Haskell's doubles to Csound's doubles
double :: Double -> D
double = prim . PrimDouble

-- | Converts Haskell's strings to Csound's strings
str :: String -> Str
str = prim . PrimString

writeVar :: (Val a) => Var -> a -> SE ()
writeVar v x = se_ $ noRate $ WriteVar v $ Fix $ unwrap x 

readVar :: (Val a) => Var -> a
readVar v = noRate $ ReadVar v

gOutVar :: Int -> Int -> Var
gOutVar instrId portId = Var GlobalVar Ar (gOutName instrId portId)
    where gOutName instrId portId = "Out" ++ show instrId ++ "_" ++ show portId

-------------------------------
-- side effects

se :: (Val a) => E -> SE a
se a = SE $ state $ \s -> 
    let x = (unwrap a) { ratedExpDepends = Just s }
    in  (wrap x, Fix $ x)

se_ :: E -> SE ()
se_ = fmap (const ()) . (se :: E -> SE E)

------------------------------------------------
-- basic destructors

getPrimUnsafe :: Val a => a -> Prim
getPrimUnsafe a = case ratedExpExp $ unwrap a of
    ExpPrim p -> p

--------------------------------------------
-- signals from primitive types

-- | Values that can be converted to signals. 
class ToSig a where
    sig :: a -> Sig
    
instance ToSig I where
    sig = wrap . unwrap

instance ToSig D where
    sig = wrap . unwrap

instance ToSig Sig where
    sig = id
    
instance ToSig Int where
    sig = sig . int
    
instance ToSig Double where
    sig = sig . double

--------------------------------------------
-- rate conversion 

setRate :: (Val a, Val b) => Rate -> a -> b
setRate r a = wrap $ (\x -> x { ratedExpRate = Just r }) $ unwrap a

-- | Forces signal to audio rate. 
ar :: Sig -> Sig
ar = setRate Ar

-- | Forces signal to control rate.
kr :: Sig -> Sig
kr = setRate Kr

-- | Converts signal to double.
ir :: Sig -> D
ir = setRate Ir

------------------------------------------------------
-- values

class Val a where
    wrap    :: RatedExp E -> a 
    unwrap  :: a -> RatedExp E

instance Val (RatedExp E) where
    wrap = id
    unwrap = id
     
instance Val E where
    wrap = Fix
    unwrap = unFix
     
instance Val Sig where
    wrap = Sig . Fix
    unwrap = unFix . unSig

instance Val I where
    wrap = I . Fix
    unwrap = unFix . unI

instance Val D where
    wrap = D . Fix
    unwrap = unFix . unD

instance Val Str where
    wrap = Str . Fix
    unwrap = unFix . unStr

instance Val Tab where
    wrap = undefined
    unwrap = prim . PrimTab

instance Val BoolSig where
    wrap = BoolSig . Fix
    unwrap = unFix . unBoolSig 

instance Val Spec where
    wrap = Spec . Fix
    unwrap = unFix . unSpec

------------------------------------------------
-- arguments

-- | The abstract type of methods for the class 'Arg'.
data ArgMethods a = ArgMethods 
    { arg :: Int -> a
    , toNote :: a -> [Prim]
    , arity :: a -> Int
    }

toArg :: Arg a => a
toArg = arg argMethods 4

-- | Defines instance of type class 'Arg' for a new type in terms of an old one.
makeArgMethods :: (Arg a) => (a -> b) -> (b -> a) -> ArgMethods b
makeArgMethods to from = ArgMethods {
    arg = to . arg argMethods,
    toNote = toNote argMethods . from,
    arity = arity argMethods . from }

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

instance Arg I where
    argMethods = ArgMethods {
        arg = p,
        toNote = pure . getPrimUnsafe,
        arity = const 1 }
         
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
              arity' (a, b) = arity argMethods a + arity argMethods b    


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


------------------------------------------------
-- tuples

-- | Describes tuples of Csound values. It's used for functions that can return 
-- several results (such as 'soundin' or 'diskin2'). Tuples can be nested. 
class CsdTuple a where
    fromCsdTuple :: a -> [E]
    toCsdTuple :: [E] -> a
    arityCsdTuple :: a -> Int

instance CsdTuple Sig where
    fromCsdTuple = return . Fix . unwrap
    toCsdTuple = wrap . unFix . head
    arityCsdTuple = const 1

instance CsdTuple I where
    fromCsdTuple = return . Fix . unwrap
    toCsdTuple = wrap . unFix . head
    arityCsdTuple = const 1

instance CsdTuple D where
    fromCsdTuple = return . Fix . unwrap
    toCsdTuple = wrap . unFix . head
    arityCsdTuple = const 1

instance CsdTuple Str where
    fromCsdTuple = return . Fix . unwrap
    toCsdTuple = wrap . unFix . head
    arityCsdTuple = const 1

instance CsdTuple Spec where
    fromCsdTuple = return . Fix . unwrap
    toCsdTuple = wrap . unFix . head
    arityCsdTuple = const 1

instance (CsdTuple a, CsdTuple b) => CsdTuple (a, b) where
    fromCsdTuple (a, b) = fromCsdTuple a ++ fromCsdTuple b
    arityCsdTuple (a, b) = arityCsdTuple a + arityCsdTuple b
    toCsdTuple xs = (a, b)
        where a = toCsdTuple $ take (arityCsdTuple a) xs
              xsb = drop (arityCsdTuple a) xs  
              b = toCsdTuple (take (arityCsdTuple b) xsb)

instance (CsdTuple a, CsdTuple b, CsdTuple c) => CsdTuple (a, b, c) where
    fromCsdTuple (a, b, c) = fromCsdTuple (a, (b, c))
    arityCsdTuple (a, b, c) = arityCsdTuple (a, (b, c))
    toCsdTuple = (\(a, (b, c)) -> (a, b, c)) . toCsdTuple 

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d) => CsdTuple (a, b, c, d) where
    fromCsdTuple (a, b, c, d) = fromCsdTuple (a, (b, c, d))
    arityCsdTuple (a, b, c, d) = arityCsdTuple (a, (b, c, d))
    toCsdTuple = (\(a, (b, c, d)) -> (a, b, c, d)) . toCsdTuple

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e) => CsdTuple (a, b, c, d, e) where
    fromCsdTuple (a, b, c, d, e) = fromCsdTuple (a, (b, c, d, e))
    arityCsdTuple (a, b, c, d, e) = arityCsdTuple (a, (b, c, d, e))
    toCsdTuple = (\(a, (b, c, d, e)) -> (a, b, c, d, e)) . toCsdTuple

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f) => CsdTuple (a, b, c, d, e, f) where
    fromCsdTuple (a, b, c, d, e, f) = fromCsdTuple (a, (b, c, d, e, f))
    arityCsdTuple (a, b, c, d, e, f) = arityCsdTuple (a, (b, c, d, e, f))
    toCsdTuple = (\(a, (b, c, d, e, f)) -> (a, b, c, d, e, f)) . toCsdTuple

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f, CsdTuple g) => CsdTuple (a, b, c, d, e, f, g) where
    fromCsdTuple (a, b, c, d, e, f, g) = fromCsdTuple (a, (b, c, d, e, f, g))
    arityCsdTuple (a, b, c, d, e, f, g) = arityCsdTuple (a, (b, c, d, e, f, g))
    toCsdTuple = (\(a, (b, c, d, e, f, g)) -> (a, b, c, d, e, f, g)) . toCsdTuple

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f, CsdTuple g, CsdTuple h) => CsdTuple (a, b, c, d, e, f, g, h) where
    fromCsdTuple (a, b, c, d, e, f, g, h) = fromCsdTuple (a, (b, c, d, e, f, g, h))
    arityCsdTuple (a, b, c, d, e, f, g, h) = arityCsdTuple (a, (b, c, d, e, f, g, h))
    toCsdTuple = (\(a, (b, c, d, e, f, g, h)) -> (a, b, c, d, e, f, g, h)) . toCsdTuple

------------------------------------------------
-- multiple outs

multiOuts :: CsdTuple a => E -> a
multiOuts exp = res
    where res = toCsdTuple $ multiOutsSection (arityCsdTuple res) exp

multiOutsSection :: Int -> E -> [E]
multiOutsSection n e = zipWith (\n r -> select n r e') [0 ..] rates
    where rates = take n $ getRates $ ratedExpExp $ unFix e          
          e' = Fix $ onExp (setMultiRate rates) $ unFix e
          
          setMultiRate rates (Tfm info xs) = Tfm (info{ infoSignature = MultiRate rates ins }) xs 
            where MultiRate _ ins = infoSignature info
            
          select n r e = withRate r $ Select r n e

getRates :: Exp a -> [Rate]
getRates (Tfm info _) = case infoSignature info of
    MultiRate outs _ -> outs
    
isMultiOutSignature :: Signature -> Bool
isMultiOutSignature x = case x of
    MultiRate _ _ -> True
    _ -> False

