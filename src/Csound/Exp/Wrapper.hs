{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language 
        TypeFamilies,
        TypeSynonymInstances,
        FlexibleInstances,
        FlexibleContexts #-}
module Csound.Exp.Wrapper(
    onE1, onE2, toExp, onExp,
    Sig, D, Str, Spec, ToSig(..),
    Sig2, Sig3, Sig4, Ksig, Amp, Cps, Iamp, Icps,
    Val(..),
    str, double, ir, ar, kr, sig,
    tfm, pref, prim, p,    
    noRate, setRate, withRate,
    getRates, isMultiOutSignature
) where

import Data.Fix
import Data.Default
import Data.String

import Csound.Exp

type Sig2 = (Sig, Sig)
type Sig3 = (Sig, Sig, Sig)
type Sig4 = (Sig, Sig, Sig, Sig)

-- | An alias for control rate signals (it's used only to clarify that 'Csound.Base.kr' was applied to the signal).
type Ksig = Sig

-- | An alias for amplitude.
type Amp = Sig

-- | An alias for cycles per second.
type Cps = Sig

-- | An alias for amplitude as number.
type Iamp = D

-- | An alias for cycles per second as number.
type Icps = D

-- | Audio or control rate signals. 
newtype Sig = Sig { unSig :: E }

-- | Doubles.
newtype D = D { unD :: E }

-- | Strings.
newtype Str = Str { unStr :: E }

-- | Spectrum of the signal (see FFT and Spectral Processing at "Csound.Opcode.Advanced"). 
newtype Spec = Spec { unSpec :: E }


------------------------------------------------------
-- values

instance IsString Str where
    fromString = str

class Val a where
    toE     :: a -> E
    fromE   :: E -> a

instance Val E          where { toE = id;       fromE = id }    
instance Val (Exp E)    where { toE = noRate;   fromE = toExp }
instance Val Sig        where { toE = unSig;    fromE = Sig }
instance Val D          where { toE = unD;      fromE = D }
instance Val Str        where { toE = unStr;    fromE = Str }
instance Val Spec       where { toE = unSpec;   fromE = Spec }

instance Val Tab    where
    fromE = TabExp
    toE x = case x of
        TabExp e -> e
        primTab -> (prim . PrimTab . Left) primTab

onE1 :: (Val a, Val b) => (E -> E) -> (a -> b)
onE1 f = fromE . f . toE

onE2 :: (Val a, Val b, Val c) => (E -> E -> E) -> (a -> b -> c)
onE2 f a b = fromE $ f (toE a) (toE b)

toExp :: Val a => a -> Exp E
toExp = ratedExpExp . unFix . toE

-- Lifts transformation of main expression
onExp :: (Exp E -> Exp E) -> E -> E
onExp f x = case unFix x of
    a -> Fix $ a{ ratedExpExp = f (ratedExpExp a) }

------------------------------------------------
-- basic constructors
  
noRate :: Val a => Exp E -> a
noRate = ratedExp Nothing
  
withRate :: Val a => Rate -> Exp E -> a
withRate r = ratedExp (Just r)

ratedExp :: Val a => Maybe Rate -> Exp E -> a
ratedExp r = fromE . Fix . RatedExp r Nothing

prim :: Val a => Prim -> a
prim = noRate . ExpPrim 

pref :: Name -> Signature -> Info
pref name signature = Info name signature Prefix Nothing

tfm :: Val a => Info -> [E] -> a
tfm info args = noRate $ Tfm info $ fmap toPrimOr args

-- variables

p :: Val a => Int -> a
p = prim . P

-- | Converts Haskell's doubles to Csound's doubles
double :: Double -> D
double = prim . PrimDouble

-- | Converts Haskell's strings to Csound's strings
str :: String -> Str
str = prim . PrimString

getRates :: MainExp a -> [Rate]
getRates (Tfm info _) = case infoSignature info of
    MultiRate outs _ -> outs
    _ -> error "Wrapper.hs:getRates - argument should be multiOut"
getRates _ = error "Wrapper.hs:getRates - argument should be Tfm-expression"
    
isMultiOutSignature :: Signature -> Bool
isMultiOutSignature x = case x of
    MultiRate _ _ -> True
    _ -> False

--------------------------------------------
-- signals from primitive types

-- | Values that can be converted to signals. 
class ToSig a where
    toSig :: a -> Sig    
    
instance ToSig D where
    toSig = sig

instance ToSig Sig where
    toSig = id
    
instance ToSig Int where
    toSig = sig . double . fromIntegral
   
instance ToSig Double where
    toSig = sig . double

--------------------------------------------
-- rate conversion 

setRate :: (Val a, Val b) => Rate -> a -> b
setRate r a = fromE $ Fix $ (\x -> x { ratedExpRate = Just r }) $ unFix $ toE a

-- | Sets rate to audio rate.
ar :: Sig -> Sig
ar = setRate Ar

-- | Sets rate to control rate.
kr :: Sig -> Sig 
kr = setRate Kr

-- | Converts signal to double.
ir :: Sig -> D
ir = setRate Ir

-- | Converts numbers to signals. It creates constant signal.
sig :: D -> Sig
sig (D a) = Sig a

--------------------------------------------
-- defaults

instance Default E   where def = prim $ PrimDouble 0

instance Default Sig  where def = fromE def    
instance Default D    where def = fromE def
instance Default Tab  where def = fromE def
instance Default Spec where def = fromE def

instance Default Str  where def = prim $ PrimString ""


