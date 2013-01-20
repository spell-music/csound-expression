module Csound.Exp where

import Control.Applicative
import Data.Monoid
import Data.Traversable
import Data.Foldable hiding (concat)

import Data.Map(Map)
import Data.Fix

import Csound.Exp.BoolExp

un = undefined

type E = Fix RatedExp

type Name = String

data RatedExp a = RatedExp (Maybe Rate) (Exp a)
    deriving (Show, Eq, Ord)

data VarType = LocalVar | GlobalVar
    deriving (Show, Eq, Ord)

data OutType = OutInstrPort Int | OutPlain | OutFile String
    deriving (Show, Eq, Ord)

onExp :: (Exp a -> Exp a) -> (RatedExp a -> RatedExp a)
onExp f (RatedExp r a) = RatedExp r (f a)


data Exp a 
    = ExpPrim Prim
    | Tfm Info [a]
    | ConvertRate Rate Rate a
    | Select Int a
    | If (CondInfo a) a a    
    | ExpBool (BoolExp a)
    | Outs [a]
    | ExpBuf BufOp [a] a    
    | Depends [a] a
    | Var VarType Rate Name
    deriving (Show, Eq, Ord)
    

data Info = Info 
    { infoName          :: Name     
    , infoSignature     :: Signature
    , infoOpcType       :: OpcType
    , infoNextSE        :: Maybe Int
    } deriving (Show, Eq, Ord)           
  
isPrefix, isInfix :: Info -> Bool

isPrefix = (Prefix ==) . infoOpcType
isInfix  = (Infix  ==) . infoOpcType
  
data OpcType = Prefix | Infix
    deriving (Show, Eq, Ord)

data Rate = Xr | Ar | Kr | Ir | Sr 
    deriving (Show, Eq, Ord, Enum)
    
data Signature 
    = SingleRate (Map Rate [Rate])
    | MultiRate 
        { outMultiRate :: [Rate] 
        , inMultiRate  :: [Rate] } 
    deriving (Show, Eq, Ord)
 
type DesiredRate = Maybe Rate

data BufOp = Delayr | Deltap | Delayw
    deriving (Show, Eq, Ord)

data Prim 
    = P Int 
    | PrimInt Int 
    | PrimDouble Double 
    | PrimFtable Ftable 
    | PrimString String 
    deriving (Show, Eq, Ord)
    
data Ftable = Ftable 
    { ftableSize    :: Int
    , ftableGen     :: Int
    , ftableArgs    :: [Double]
    } deriving (Show, Eq, Ord)


-------------------------------------------------------
-- instances for cse

instance Functor RatedExp where
    fmap f (RatedExp r a) = RatedExp r $ fmap f a

instance Foldable RatedExp where
    foldMap f (RatedExp _ a) = foldMap f a
    
instance Traversable RatedExp where
    traverse f (RatedExp r a) = RatedExp r <$> traverse f a

instance Functor Exp where
    fmap f x = case x of
        ExpPrim p -> ExpPrim p
        Tfm t xs -> Tfm t $ map f xs
        ConvertRate ra rb a -> ConvertRate ra rb $ f a
        Select n a -> Select n $ f a
        If info a b -> If (fmap f info) (f a) (f b)
        ExpBool a -> ExpBool $ fmap f a
        Outs as -> Outs $ fmap f as
        ExpBuf op deps a -> ExpBuf op (fmap f deps) $ f a        
        Depends deps a -> Depends (fmap f deps) $ f a
        Var ty r name -> Var ty r name

instance Foldable Exp where
    foldMap f x = case x of
        ExpPrim p -> mempty
        Tfm t xs -> foldMap f xs
        ConvertRate ra rb a -> f a
        Select n a -> f a
        If info a b -> foldMap f info <> f a <> f b
        ExpBool a -> foldMap f a
        Outs as -> foldMap f as
        ExpBuf op deps a -> foldMap f deps <> f a
        Depends deps a -> foldMap f deps <> f a
        Var ty r name -> mempty
        
instance Traversable Exp where
    traverse f x = case x of
        ExpPrim p -> pure $ ExpPrim p
        Tfm t xs -> Tfm t <$> traverse f xs
        ConvertRate ra rb a -> ConvertRate ra rb <$> f a
        Select n a -> Select n <$> f a
        If info a b -> If <$> traverse f info <*> f a <*> f b
        ExpBool a -> ExpBool <$> traverse f a
        Outs as -> Outs <$> traverse f as
        ExpBuf op deps a -> ExpBuf op <$> traverse f deps <*> f a
        Depends deps a -> Depends <$> traverse f deps <*> f a
        Var ty r name -> pure $ Var ty r name


