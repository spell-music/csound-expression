module Csound.Exp(
    E, RatedExp(..), RatedVar(..), onExp, Exp(..), Name,
    VarType(..), Var(..), Info(..), OpcType(..), Rate(..), 
    Signature(..), isProcedure, isInfix, isPrefix,    
    Prim(..), Tab(..),
    Inline(..), InlineExp(..), PreInline(..),
    BoolExp, CondInfo, CondOp(..), isTrue, isFalse,    
    NumExp, NumOp(..)    
) where

import Control.Applicative
import Data.Monoid
import Data.Traversable
import Data.Foldable hiding (concat)

import Data.Map(Map)
import qualified Data.IntMap as IM
import Data.Fix

-- | The inner representation of csound expressions.
type E = Fix RatedExp

type Name = String

data RatedExp a = RatedExp 
    { ratedExpRate      :: Maybe Rate
    , ratedExpDepends   :: Maybe a
    , ratedExpExp       :: Exp a
    } deriving (Show, Eq, Ord)

data RatedVar = RatedVar 
    { ratedVarRate :: Rate 
    , ratedVarId   :: Int 
    } deriving (Show)

onExp :: (Exp a -> Exp a) -> RatedExp a -> RatedExp a
onExp f a = a{ ratedExpExp = f (ratedExpExp a) }

data VarType = LocalVar | GlobalVar
    deriving (Show, Eq, Ord)

data Exp a 
    = ExpPrim Prim
    | Tfm Info [a]
    | ConvertRate Rate Rate a
    | Select Rate Int a
    | If (CondInfo a) a a    
    | ExpBool (BoolExp a)
    | ExpNum (NumExp a)
    | ReadVar Var
    | WriteVar Var a    
    deriving (Show, Eq, Ord)  

data Var 
    = Var
        { varType :: VarType
        , varRate :: Rate
        , varName :: Name } 
    | VarVerbatim 
        { varRate :: Rate
        , varName :: Name        
        } deriving (Show, Eq, Ord)       
        

data Info = Info 
    { infoName          :: Name     
    , infoSignature     :: Signature
    , infoOpcType       :: OpcType
    , infoNextSE        :: Maybe Int
    } deriving (Show, Eq, Ord)           
  
isPrefix, isInfix, isProcedure :: Info -> Bool

isPrefix = (Prefix ==) . infoOpcType
isInfix  = (Infix  ==) . infoOpcType
isProcedure = (Procedure ==) . infoOpcType
  
data OpcType = Prefix | Infix | Procedure
    deriving (Show, Eq, Ord)

-- | The Csound rates.
data Rate = Xr | Ar | Kr | Ir | Sr | Fr
    deriving (Show, Eq, Ord, Enum, Bounded)
    
data Signature 
    = SingleRate (Map Rate [Rate])
    | MultiRate 
        { outMultiRate :: [Rate] 
        , inMultiRate  :: [Rate] } 
    deriving (Show, Eq, Ord)
 
data Prim 
    = P Int 
    | PString Int       -- >> p-string: 
    | PrimInt Int 
    | PrimDouble Double 
    | PrimTab Tab 
    | PrimString String 
    deriving (Show, Eq, Ord)
    
-- | Csound f-tables. You can make a value of 'Tab' with the function 'gen'.
data Tab = Tab 
    { tabSize    :: Int
    , tabGen     :: Int
    , tabArgs    :: [Double]
    } deriving (Show, Eq, Ord)

------------------------------------------------------------
-- types for arithmetic and boolean expressions

data Inline a b = Inline 
    { inlineExp :: InlineExp a
    , inlineEnv :: IM.IntMap b    
    } deriving (Show, Eq, Ord)

data InlineExp a
    = InlinePrim Int
    | InlineExp a [InlineExp a]
    deriving (Show, Eq, Ord)

data PreInline a b = PreInline a [b]
    deriving (Show, Eq, Ord)

-- booleans

type BoolExp a = PreInline CondOp a
type CondInfo a = Inline CondOp a

data CondOp  
    = TrueOp | FalseOp | Not | And | Or
    | Equals | NotEquals | Less | Greater | LessEquals | GreaterEquals
    deriving (Show, Eq, Ord)    

isTrue, isFalse :: CondInfo a -> Bool

isTrue  = isCondOp TrueOp
isFalse = isCondOp FalseOp

isCondOp op = maybe False (op == ) . getCondInfoOp

getCondInfoOp :: CondInfo a -> Maybe CondOp
getCondInfoOp x = case inlineExp x of
    InlineExp op _ -> Just op
    _ -> Nothing

-- numbers

type NumExp a = PreInline NumOp a

data NumOp 
    = Add | Sub | Neg | Mul | Div
    | Pow | Mod 
    | Sin | Cos | Sinh | Cosh | Tan | Tanh | Sininv | Cosinv | Taninv
    | Abs | Ceil | ExpOp | Floor | Frac| IntOp | Log | Log10 | Logbtwo | Round | Sqrt    
    | Ampdb | Ampdbfs | Dbamp | Dbfsamp 
    | Cpspch
    deriving (Show, Eq, Ord)

-------------------------------------------------------
-- instances for cse

instance Functor RatedExp where
    fmap f (RatedExp r d a) = RatedExp r (fmap f d) (fmap f a)

instance Foldable RatedExp where
    foldMap f (RatedExp _ d a) = foldMap f d <> foldMap f a
    
instance Traversable RatedExp where
    traverse f (RatedExp r d a) = RatedExp r <$> traverse f d <*> traverse f a

instance Functor Exp where
    fmap f x = case x of
        ExpPrim p -> ExpPrim p
        Tfm t xs -> Tfm t $ map f xs
        ConvertRate ra rb a -> ConvertRate ra rb $ f a
        Select r n a -> Select r n $ f a
        If info a b -> If (fmap f info) (f a) (f b)
        ExpBool a -> ExpBool $ fmap f a
        ExpNum  a -> ExpNum  $ fmap f a
        ReadVar v -> ReadVar v
        WriteVar v a -> WriteVar v (f a)        


instance Foldable Exp where
    foldMap f x = case x of
        ExpPrim p -> mempty
        Tfm t xs -> foldMap f xs
        ConvertRate ra rb a -> f a
        Select r n a -> f a
        If info a b -> foldMap f info <> f a <> f b
        ExpBool a -> foldMap f a
        ExpNum  a -> foldMap f a
        ReadVar v -> mempty
        WriteVar v a -> f a
        
instance Traversable Exp where
    traverse f x = case x of
        ExpPrim p -> pure $ ExpPrim p
        Tfm t xs -> Tfm t <$> traverse f xs
        ConvertRate ra rb a -> ConvertRate ra rb <$> f a
        Select r n a -> Select r n <$> f a
        If info a b -> If <$> traverse f info <*> f a <*> f b
        ExpBool a -> ExpBool <$> traverse f a
        ExpNum  a -> ExpNum  <$> traverse f a
        ReadVar v -> pure $ ReadVar v
        WriteVar v a -> WriteVar v <$> f a


instance Functor (Inline a) where
    fmap f a = a{ inlineEnv = fmap f $ inlineEnv a }

instance Foldable (Inline a) where
    foldMap f a = foldMap f $ inlineEnv a

instance Traversable (Inline a) where
    traverse f (Inline a b) = Inline a <$> traverse f b

instance Functor (PreInline a) where
    fmap f (PreInline op as) = PreInline op $ fmap f as

instance Foldable (PreInline a) where
    foldMap f (PreInline _ as) = foldMap f as

instance Traversable (PreInline a) where
    traverse f (PreInline op as) = PreInline op <$> traverse f as



-- comments
-- 
-- p-string 
--
--    separate p-param for strings (we need it to read strings from global table) 
--    Csound doesn't permits us to use more than four string params so we need to
--    keep strings in the global table and use `strget` to read them

