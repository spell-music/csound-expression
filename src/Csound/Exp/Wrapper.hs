{-# Language 
        TypeSynonymInstances,
        FlexibleInstances #-}
module Csound.Exp.Wrapper where

import Control.Applicative
import Data.String
import Data.Fix
import Control.Monad.Trans.State

import Csound.Exp

type Channel = Int

newtype Sig = Sig { unSig :: E }

newtype Int' = Int' { unInt' :: E }

newtype Double' = Double' { unDouble' :: E }

newtype String' = String' { unString' :: E }

newtype SE a = SE { unSE :: E }

newtype BoolSig = BoolSig { unBoolSig :: E }

------------------------------------------------
-- shortcuts

type D = Double'
type I = Int'
type S = String'
type Tab = Ftable

------------------------------------------------
-- basic constructors
  
noRate :: Val a => Exp E -> a
noRate = ratedExp Nothing
  
withRate :: Val a => Rate -> Exp E -> a
withRate r = ratedExp (Just r)

ratedExp :: Val a => Maybe Rate -> Exp E -> a
ratedExp r = wrap . RatedExp r

prim :: Val a => Prim -> a
prim = wrap . noRate . ExpPrim 
 

pref :: Name -> Signature -> Info
pref name signature = Info name signature Prefix Nothing

inf :: Name -> Signature -> Info
inf name signature = Info name signature Infix Nothing
  
tfm :: Val a => Info -> [RatedExp E] -> a
tfm info args = wrap $ noRate $ Tfm info $ map Fix args

opc :: Val a => Name -> Signature -> [RatedExp E] -> a
opc name signature = tfm (pref name signature)

opr :: Val a => Name -> Signature -> [RatedExp E] -> a
opr name signature = tfm (inf name signature)

gvar, var :: Val a => Rate -> Name -> a

var  = mkVar LocalVar 
gvar = mkVar GlobalVar

mkVar :: Val a => VarType -> Rate -> String -> a
mkVar ty rate name = wrap $ noRate $ Var ty rate name

p :: Init a => Int -> a
p = prim . P

int :: Int -> Int'
int = prim . PrimInt

double :: Double -> Double'
double = prim . PrimDouble

str :: String -> String' 
str = prim . PrimString

------------------------------------------------
-- basic destructors

getPrimUnsafe :: Val a => a -> Prim
getPrimUnsafe a = case unwrap a of
    RatedExp _ (ExpPrim p) -> p



--------------------------------------------
-- signals from primitive types

class ToSig a where
    sig :: a -> Sig
    
instance ToSig Int' where
    sig = wrap . unwrap

instance ToSig Double' where
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
setRate r a = wrap $ phi $ unwrap a
    where phi (RatedExp _ exp) = RatedExp (Just r) exp

ar :: ToSig a => a -> Sig
ar = setRate Ar . sig

kr :: ToSig a => a -> Sig
kr = setRate Kr . sig

ir :: Sig -> Double'
ir = setRate Ir

------------------------------------------------------
-- inits

-- types that can be used in score

class Val a => Init a where

instance Init Int' where
instance Init Double' where
instance Init String' where
instance Init Ftable where

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

instance Val Int' where
    wrap = Int' . Fix
    unwrap = unFix . unInt'

instance Val Double' where
    wrap = Double' . Fix
    unwrap = unFix . unDouble'

instance Val String' where
    wrap = String' . Fix
    unwrap = unFix . unString'

instance Val Ftable where
    wrap = un
    unwrap = prim . PrimFtable

instance Val a => Val (SE a) where
    wrap = SE . Fix
    unwrap = unFix . unSE

instance Val BoolSig where
    wrap = BoolSig . Fix
    unwrap = unFix . unBoolSig 

------------------------------------------------
-- arguments

class Arg a where
    arg :: a  
    toNote :: a -> [Prim]

instance Arg Int' where
    arg = p 4
    toNote = pure . getPrimUnsafe 
         
instance Arg Double' where
    arg = p 4
    toNote = pure . getPrimUnsafe

instance Arg String' where
    arg = p 4
    toNote = pure . getPrimUnsafe

instance Arg Ftable where
    arg = p 4
    toNote = pure . getPrimUnsafe

instance (Init a, Init b, Arg a, Arg b) => Arg (a, b) where
    arg = (p 4, p 5)
    toNote (a, b) = concat [toNote a, toNote b]

instance (Init a, Init b, Init c, Arg a, Arg b, Arg c) => Arg (a, b, c) where
    arg = (p 4, p 5, p 6)
    toNote (a, b, c) = concat [toNote a, toNote b, toNote c]

instance (Init a, Init b, Init c, Init d, Arg a, Arg b, Arg c, Arg d) => Arg (a, b, c, d) where
    arg = (p 4, p 5, p 6, p 7)
    toNote (a, b, c, d) = concat [toNote a, toNote b, toNote c, toNote d]

------------------------------------------------
-- multiple outs

class MultiOut a where
    multiOuts :: E -> a

instance MultiOut Sig where
    multiOuts x = case multiOutsSection 1 x of
        [a1] -> a1

instance MultiOut (Sig, Sig) where
    multiOuts x = case multiOutsSection 2 x of
        [a1, a2] -> (a1, a2)
    
instance MultiOut (Sig, Sig, Sig) where
    multiOuts x = case multiOutsSection 3 x of
        [a1, a2, a3] -> (a1, a2, a3)

instance MultiOut (Sig, Sig, Sig, Sig) where
    multiOuts x = case multiOutsSection 4 x of
        [a1, a2, a3, a4] -> (a1, a2, a3, a4)
        

multiOutsSection :: Int -> E -> [Sig]
multiOutsSection n e = zipWith (\n r -> select n r e') [0 ..] rates
    where rates = take n $ getRates exp
          RatedExp _ exp = unFix e 
          e' = Fix $ onExp (setMultiRate rates) $ unFix e
          
          setMultiRate rates (Tfm info xs) = Tfm (info{ infoSignature = MultiRate rates ins }) xs 
            where MultiRate _ ins = infoSignature info
            
          select n r e = withRate r $ Select n e

getRates :: Exp a -> [Rate]
getRates (Tfm info _) = case infoSignature info of
    MultiRate outs _ -> outs
    
isMultiOutSignature :: Signature -> Bool
isMultiOutSignature x = case x of
    MultiRate _ _ -> True
    _ -> False

------------------------------------------------
-- CsdTuple

mapCsdTuple :: CsdTuple a => (E -> E) -> a -> a
mapCsdTuple f x = evalState toTuple $ fmap f $ fromTuple x

class CsdTuple a where
    toTuple :: State [E] a
    fromTuple :: a -> [E]

instance CsdTuple Sig where
    toTuple   = state $ \as -> (Sig $ head as, tail as)
    fromTuple = return . unSig

instance (CsdTuple a, CsdTuple b) => CsdTuple (a, b) where
    toTuple = (,) <$> toTuple <*> toTuple
    fromTuple (a, b) = fromTuple a ++ fromTuple b

instance (CsdTuple a, CsdTuple b, CsdTuple c) => CsdTuple (a, b, c) where
    toTuple = (,,) <$> toTuple <*> toTuple <*> toTuple
    fromTuple (a, b, c) = fromTuple a ++ fromTuple b ++ fromTuple c

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d) => CsdTuple (a, b, c, d) where
    toTuple = (,,,) <$> toTuple <*> toTuple <*> toTuple <*> toTuple
    fromTuple (a, b, c, d) = fromTuple a ++ fromTuple b ++ fromTuple c ++ fromTuple d



