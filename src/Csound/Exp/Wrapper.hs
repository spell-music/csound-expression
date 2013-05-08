{-# Language 
        TypeFamilies,
        TypeSynonymInstances,
        FlexibleInstances,
        FlexibleContexts #-}
module Csound.Exp.Wrapper(
    onE1, onE2, toExp, onExp,
    Outs, Sig, D, Str, Spec, ToSig(..),
    Sig2, Sig3, Sig4, Ksig, Amp, Cps, Iamp, Icps,
    SE, History(..), se, se_, runSE, execSE, newVar,
    ifBegin, ifEnd, elseIfBegin, elseBegin,
    Val(..),
    str, double, ir, ar, kr, sig,
    tfm, pref, prim, p,    
    noRate, setRate, withRate,
    getRates, isMultiOutSignature,
    readVar, writeVar, gOutVar,
    Channel
) where

import Control.Applicative
import Control.Monad(ap, join)
import Control.Monad.Trans.State
import Data.Fix
import Data.Default
import Data.Maybe(fromJust)

import Csound.Exp

type Channel = Int

type Outs = SE [Sig]
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

------------------------------------------------
-- side effects

-- | Csound's synonym for 'IO'-monad. 'SE' means Side Effect. 
-- You will bump into 'SE' trying to read and write to delay lines,
-- making random signals or trying to save your audio to file. 
-- Instrument is expected to return a value of @SE [Sig]@. 
-- So it's okay to do some side effects when playing a note.
newtype SE a = SE { unSE :: State History a }

data History = History
    { expDependency :: Maybe E
    , newVarId      :: Int }

instance Default History where
    def = History Nothing 0

instance Functor SE where
    fmap f = SE . fmap f . unSE

instance Applicative SE where
    pure = return
    (<*>) = ap

instance Monad SE where
    return = SE . return
    ma >>= mf = SE $ unSE ma >>= unSE . mf

runSE :: SE a -> (a, History)
runSE a = runState (unSE a) def

execSE :: SE a -> E
execSE = fromJust . expDependency . snd . runSE

se :: (Val a) => E -> SE a
se a = SE $ state $ \s -> 
    let x = Fix $ (unFix a) { ratedExpDepends = expDependency s }
    in  (fromE x, s{ expDependency = Just x } )

se_ :: E -> SE ()
se_ = fmap (const ()) . (se :: E -> SE E)

newVar :: Rate -> SE Var
newVar rate = SE $ state $ \s -> 
    (Var LocalVar rate ("var" ++ show (newVarId s)), s{ newVarId = succ (newVarId s) })

ifBegin :: Val a => a -> SE ()
ifBegin = withCond IfBegin

elseIfBegin :: Val a => a -> SE ()
elseIfBegin = withCond ElseIfBegin

elseBegin :: SE ()
elseBegin = stmtOnly ElseBegin

ifEnd :: SE ()
ifEnd = stmtOnly IfEnd

stmtOnly stmt = se_ $ fromE $ noRate stmt

withCond :: Val a => (E -> MainExp E) -> a -> SE ()
withCond stmt cond = se_ $ fromE $ noRate $ fmap (PrimOr . Right) $ stmt (toE cond)

------------------------------------------------------
-- values

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


fromVal :: (Val a, Val b) => a -> b
fromVal = fromE . toE

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

inf :: Name -> Signature -> Info
inf name signature = Info name signature Infix Nothing
  
tfm :: Val a => Info -> [E] -> a
tfm info args = noRate $ Tfm info $ fmap toPrimOr args

gvar, var :: Val a => Rate -> Name -> a

var  = mkVar LocalVar 
gvar = mkVar GlobalVar

mkVar :: Val a => VarType -> Rate -> String -> a
mkVar ty rate name = noRate $ ReadVar (Var ty rate name)

p :: Val a => Int -> a
p = prim . P

-- | Converts Haskell's doubles to Csound's doubles
double :: Double -> D
double = prim . PrimDouble

-- | Converts Haskell's strings to Csound's strings
str :: String -> Str
str = prim . PrimString

writeVar :: (Val a) => Var -> a -> SE ()
writeVar v x = se_ $ noRate $ WriteVar v $ toPrimOr $ toE x 

readVar :: (Val a) => Var -> a
readVar v = noRate $ ReadVar v

gOutVar :: Int -> Int -> Var
gOutVar instrId portId = Var GlobalVar Ar (gOutName instrId portId)
    where gOutName instrId portId = "Out" ++ show instrId ++ "_" ++ show portId

getRates :: MainExp a -> [Rate]
getRates (Tfm info _) = case infoSignature info of
    MultiRate outs _ -> outs
    
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


