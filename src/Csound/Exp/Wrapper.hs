{-# Language 
        TypeFamilies,
        TypeSynonymInstances,
        FlexibleInstances,
        FlexibleContexts #-}
module Csound.Exp.Wrapper(
    toE, fromE,
    Out(..), Outs, Sig, D, Str, BoolSig(..), BoolD(..), Spec, ToSig(..),
    Sig2, Sig3, Sig4, Amp, Cps, Iamp, Icps,
    SE, se, se_, runSE, execSE,
    Arg(..), ArgMethods(..), toArg, makeArgMethods,
    CsdTuple(..), multiOuts,
    Val(..),
    str, double, ir, ar, kr, sig,
    tfm, pref, prim, p,
    isMultiOutSignature,
    noRate, setRate, 
    getRates, tabMap, updateTabSize, defineInstrTabs, substInstrTabs, defineNoteTabs, substNoteTabs,
    stringMap, substNoteStrs,
    readVar, writeVar, gOutVar,
    Channel
) where

import Control.Applicative
import Control.Monad(ap, join)
import Control.Monad.Trans.State

import Data.List(nub, splitAt)
import Data.String
import Data.Fix
import Control.Monad.Trans.State
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Foldable(foldMap)

import Csound.Exp

type Channel = Int

type Outs = SE [Sig]
type Sig2 = (Sig, Sig)
type Sig3 = (Sig, Sig, Sig)
type Sig4 = (Sig, Sig, Sig, Sig)

-- | An alias for amplitude.
type Amp = Sig

-- | An alias for cycles per second.
type Cps = Sig

-- | An alias for amplitude as number.
type Iamp = D

-- | An alias for cycles per second as number.
type Icps = D

-- | Converts a value to the private representation.
toE :: Val a => a -> E
toE = Fix . unwrap

-- | Constructs a value from the private representation.
fromE :: Val a => E -> a
fromE = wrap . unFix

-- | Output of the instrument.
class CsdTuple (NoSE a) => Out a where
    type NoSE a :: *
    toOut :: a -> SE [Sig]
    fromOut :: [Sig] -> a

-- | Audio or control rate signals. 
newtype Sig = Sig { unSig :: E }

-- | Doubles.
newtype D = D { unD :: E }

-- | Strings.
newtype Str = Str { unStr :: E }

-- | Boolean signals. 
newtype BoolSig = BoolSig { unBoolSig :: E }

-- | Boolean constants. 
newtype BoolD = BoolD { unBoolD :: E }

-- | Spectrum of the signal (see FFT and Spectral Processing at "Csound.Opcode.Advanced"). 
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
tfm info args = wrap $ noRate $ Tfm info $ map (toPrimOr . Fix) args

gvar, var :: Val a => Rate -> Name -> a

var  = mkVar LocalVar 
gvar = mkVar GlobalVar

mkVar :: Val a => VarType -> Rate -> String -> a
mkVar ty rate name = wrap $ noRate $ ReadVar (Var ty rate name)

p :: Val a => Int -> a
p = prim . P

-- | Converts Haskell's doubles to Csound's doubles
double :: Double -> D
double = prim . PrimDouble

-- | Converts Haskell's strings to Csound's strings
str :: String -> Str
str = prim . PrimString

writeVar :: (Val a) => Var -> a -> SE ()
writeVar v x = se_ $ noRate $ WriteVar v $ toPrimOr $ Fix $ unwrap x 

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
-- basic extractors

getPrimUnsafe :: Val a => a -> Prim
getPrimUnsafe a = case ratedExpExp $ unwrap a of
    ExpPrim p -> p

tabMap :: [E] -> [Prim] -> TabMap
tabMap es ps = M.fromList $ zip (nub $ (getPrimTabs =<< ps) ++ (getInstrTabs =<< es)) [1 ..]
    
getInstrTabs :: E -> [LowTab]
getInstrTabs = cata $ \re -> (maybe [] id $ ratedExpDepends re) ++ case fmap fromPrimOr $ ratedExpExp re of    
    ExpPrim p -> getPrimTabs p
    Tfm _ as -> concat as
    ConvertRate _ _ a -> a
    ExpNum a -> foldMap id a
    Select _ _ a -> a
    If info a b -> foldMap id info ++ a ++ b
    ReadVar _ -> []
    WriteVar _ a -> a
    where fromPrimOr x = case unPrimOr x of
            Left  p -> getPrimTabs p
            Right a -> a

getPrimTabs :: Prim -> [LowTab]
getPrimTabs x = case x of
    PrimTab (Right t) -> [t]
    _ -> []

substPrimTab :: TabMap -> Prim -> Prim
substPrimTab m x = case x of 
    PrimTab (Right tab) -> PrimInt (m M.! tab)
    _ -> x

substInstrTabs :: TabMap -> E -> E
substInstrTabs m = cata $ \re -> Fix $ re { ratedExpExp = fmap phi $ ratedExpExp re }
    where phi x = case unPrimOr x of
            Left p -> PrimOr $ Left $ substPrimTab m p
            _ -> x 

substNoteTabs :: TabMap -> Note -> Note
substNoteTabs m = fmap (substPrimTab m)

defineInstrTabs :: TabFi -> E -> E
defineInstrTabs n = cata $ \re -> Fix $ re { ratedExpExp = fmap phi $ ratedExpExp re }
    where phi x = case unPrimOr x of
            Left p -> PrimOr $ Left $ definePrimTab n p
            _ -> x 

definePrimTab :: TabFi -> Prim -> Prim
definePrimTab n x = case x of
    PrimTab (Left tab) -> PrimTab (Right $ defineTab n tab)
    _ -> x

defineNoteTabs :: TabFi -> Note -> Note
defineNoteTabs n = fmap (definePrimTab n)

defineTab :: TabFi -> Tab -> LowTab
defineTab tabFi tab = LowTab size (tabGen tab) args
    where size = defineTabSize (getTabSizeBase tabFi tab) (tabSize tab)
          args = defineTabArgs size (tabArgs tab)

getTabSizeBase :: TabFi -> Tab -> Int
getTabSizeBase tf tab = IM.findWithDefault (tabFiBase tf) (tabGen tab) (tabFiGens tf)

defineTabArgs :: Int -> TabArgs -> [Double] 
defineTabArgs size args = case args of
    ArgsPlain as -> as 
    ArgsRelative as -> fromRelative size as
    where fromRelative n as = substEvens (mkRelative n $ getEvens as) as
          getEvens xs = case xs of
            [] -> []
            a:[] -> []
            a:b:as -> b : getEvens as
            
          substEvens evens xs = case (evens, xs) of
            ([], xs) -> xs
            (es, []) -> []
            (e:es, a:b:as) -> a : e : substEvens es as
            
          mkRelative n as = fmap (fromIntegral . round . (s * )) as
            where s = fromIntegral n / sum as
            

defineTabSize :: Int -> TabSize -> Int
defineTabSize base x = case x of
       SizePlain n -> n
       SizeDegree guardPoint degree ->          
                byGuardPoint guardPoint $
                byDegree base degree
    where byGuardPoint guardPoint 
            | guardPoint = (+ 1)
            | otherwise  = id
            
          byDegree base n = 2 ^ max 0 (base + n) 

updateTabSize :: (TabSize -> TabSize) -> Tab -> Tab
updateTabSize phi x = case x of
    TabExp _ -> error "you can change size only for primitive tables (made with gen-routines)"
    primTab  -> primTab{ tabSize = phi $ tabSize primTab }


------------------------------------------------------------------
-- render strings

stringMap :: [Prim] -> StringMap
stringMap as = M.fromList $ zip (nub $ primStrings =<< as) [1 .. ]
    where primStrings x = case x of
              PrimString s -> [s]
              _ -> []

substNoteStrs :: StringMap -> Note -> Note
substNoteStrs m = fmap (substPrimStrs m)

substPrimStrs :: StringMap -> Prim -> Prim
substPrimStrs strs x = case x of
    PrimString s -> PrimInt $ strs M.! s
    _ -> x



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
setRate r a = wrap $ (\x -> x { ratedExpRate = Just r }) $ unwrap a

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

instance Val D where
    wrap = D . Fix
    unwrap = unFix . unD

instance Val Str where
    wrap = Str . Fix
    unwrap = unFix . unStr

instance Val Tab where
    wrap = TabExp . Fix
    unwrap x = case x of
        TabExp e -> unFix e
        primTab -> (prim . PrimTab . Left) primTab

instance Val BoolSig where
    wrap = BoolSig . Fix
    unwrap = unFix . unBoolSig 

instance Val BoolD where
    wrap = BoolD . Fix
    unwrap = unFix . unBoolD 

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

instance CsdTuple D where
    fromCsdTuple = return . Fix . unwrap
    toCsdTuple = wrap . unFix . head
    arityCsdTuple = const 1

instance CsdTuple Tab where
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
    arityCsdTuple x = let (a, b) = proxy x in arityCsdTuple a + arityCsdTuple b
        where proxy :: (a, b) -> (a, b)
              proxy = const (undefined, undefined)  
    toCsdTuple xs = (a, b)
        where a = toCsdTuple $ take (arityCsdTuple a) xs
              xsb = drop (arityCsdTuple a) xs  
              b = toCsdTuple (take (arityCsdTuple b) xsb)

instance (CsdTuple a, CsdTuple b, CsdTuple c) => CsdTuple (a, b, c) where
    fromCsdTuple (a, b, c) = fromCsdTuple (a, (b, c))
    arityCsdTuple = arityCsdTuple . proxy
        where proxy :: (a, b, c) -> (a, (b, c))
              proxy = const undefined  
    toCsdTuple = (\(a, (b, c)) -> (a, b, c)) . toCsdTuple 

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d) => CsdTuple (a, b, c, d) where
    fromCsdTuple (a, b, c, d) = fromCsdTuple (a, (b, c, d))
    arityCsdTuple = arityCsdTuple . proxy
        where proxy :: (a, b, c, d) -> (a, (b, c, d))
              proxy = const undefined  
    toCsdTuple = (\(a, (b, c, d)) -> (a, b, c, d)) . toCsdTuple

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e) => CsdTuple (a, b, c, d, e) where
    fromCsdTuple (a, b, c, d, e) = fromCsdTuple (a, (b, c, d, e))
    arityCsdTuple = arityCsdTuple . proxy
        where proxy :: (a, b, c, d, e) -> (a, (b, c, d, e))
              proxy = const undefined  
    toCsdTuple = (\(a, (b, c, d, e)) -> (a, b, c, d, e)) . toCsdTuple

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f) => CsdTuple (a, b, c, d, e, f) where
    fromCsdTuple (a, b, c, d, e, f) = fromCsdTuple (a, (b, c, d, e, f))
    arityCsdTuple = arityCsdTuple . proxy
        where proxy :: (a, b, c, d, e, f) -> (a, (b, c, d, e, f))
              proxy = const undefined  
    toCsdTuple = (\(a, (b, c, d, e, f)) -> (a, b, c, d, e, f)) . toCsdTuple

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f, CsdTuple g) => CsdTuple (a, b, c, d, e, f, g) where
    fromCsdTuple (a, b, c, d, e, f, g) = fromCsdTuple (a, (b, c, d, e, f, g))
    arityCsdTuple = arityCsdTuple . proxy
        where proxy :: (a, b, c, d, e, f, g) -> (a, (b, c, d, e, f, g))
              proxy = const undefined  
    toCsdTuple = (\(a, (b, c, d, e, f, g)) -> (a, b, c, d, e, f, g)) . toCsdTuple

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f, CsdTuple g, CsdTuple h) => CsdTuple (a, b, c, d, e, f, g, h) where
    fromCsdTuple (a, b, c, d, e, f, g, h) = fromCsdTuple (a, (b, c, d, e, f, g, h))
    arityCsdTuple = arityCsdTuple . proxy
        where proxy :: (a, b, c, d, e, f, g, h) -> (a, (b, c, d, e, f, g, h))
              proxy = const undefined  
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
            
          select n r e = withRate r $ Select r n (PrimOr $ Right e)

getRates :: MainExp a -> [Rate]
getRates (Tfm info _) = case infoSignature info of
    MultiRate outs _ -> outs
    
isMultiOutSignature :: Signature -> Bool
isMultiOutSignature x = case x of
    MultiRate _ _ -> True
    _ -> False

------------------------------------------------
-- instrument outs

instance Out Sig where
    type NoSE Sig = Sig
    toOut = return . return
    fromOut = head  

instance (Out a, CsdTuple a) => Out (SE a) where
    type NoSE (SE a) = a
    toOut = join . fmap toOut
    fromOut = return . fromOut


instance (CsdTuple a, CsdTuple b, Out a, Out b) => Out (a, b) where
    type NoSE (a, b) = (NoSE a, NoSE b)
    toOut (a, b) = liftA2 (++) (toOut a) (toOut b)
    fromOut = toCsdTuple . fmap toE

    
instance (CsdTuple a, CsdTuple b, CsdTuple c, Out a, Out b, Out c) => Out (a, b, c) where
    type NoSE (a, b, c) = (NoSE a, NoSE b, NoSE c)
    toOut (a, b, c) = toOut (a, (b, c))
    fromOut = toCsdTuple . fmap toE
    
instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, Out a, Out b, Out c, Out d) => Out (a, b, c, d) where
    type NoSE (a, b, c, d) = (NoSE a, NoSE b, NoSE c, NoSE d)
    toOut (a, b, c, d) = toOut (a, (b, c, d))
    fromOut = toCsdTuple . fmap toE
    
instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, Out a, Out b, Out c, Out d, Out e) => Out (a, b, c, d, e) where
    type NoSE (a, b, c, d, e) = (NoSE a, NoSE b, NoSE c, NoSE d, NoSE e)
    toOut (a, b, c, d, e) = toOut (a, (b, c, d, e))
    fromOut = toCsdTuple . fmap toE
    
instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f, Out a, Out b, Out c, Out d, Out e, Out f) => Out (a, b, c, d, e, f) where
    type NoSE (a, b, c, d, e, f) = (NoSE a, NoSE b, NoSE c, NoSE d, NoSE e, NoSE f)
    toOut (a, b, c, d, e, f) = toOut (a, (b, c, d, e, f))
    fromOut = toCsdTuple . fmap toE
    
instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f, CsdTuple g, Out a, Out b, Out c, Out d, Out e, Out f, Out g) => Out (a, b, c, d, e, f, g) where
    type NoSE (a, b, c, d, e, f, g) = (NoSE a, NoSE b, NoSE c, NoSE d, NoSE e, NoSE f, NoSE g)
    toOut (a, b, c, d, e, f, g) = toOut (a, (b, c, d, e, f, g))
    fromOut = toCsdTuple . fmap toE
    
instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f, CsdTuple g, CsdTuple h, Out a, Out b, Out c, Out d, Out e, Out f, Out g, Out h) => Out (a, b, c, d, e, f, g, h) where
    type NoSE (a, b, c, d, e, f, g, h) = (NoSE a, NoSE b, NoSE c, NoSE d, NoSE e, NoSE f, NoSE g, NoSE h)
    toOut (a, b, c, d, e, f, g, h) = toOut (a, (b, c, d, e, f, g, h))
    fromOut = toCsdTuple . fmap toE

       
 



