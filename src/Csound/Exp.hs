{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Csound.Exp(
    E, RatedExp(..), RatedVar(..), onExp, Exp, toPrimOr, PrimOr(..), MainExp(..), Name,
    VarType(..), Var(..), Info(..), OpcType(..), Rate(..), 
    Signature(..), isProcedure, isInfix, isPrefix,    
    Prim(..), LowTab(..), Tab(..), TabSize(..), TabArgs(..), TabMap, TabFi(..),
    Inline(..), InlineExp(..), PreInline(..),
    BoolExp, CondInfo, CondOp(..), isTrue, isFalse,    
    NumExp, NumOp(..), Msg(..), Note, Event(..), eventEnd,   
) where

import Control.Applicative
import Data.Monoid
import Data.Traversable
import Data.Foldable hiding (concat)

import Data.Default

import Data.Map(Map)
import qualified Data.IntMap as IM
import qualified Data.Map    as M
import Data.Fix

-- | The inner representation of csound expressions.
type E = Fix RatedExp

type Name = String

data RatedExp a = RatedExp 
    { ratedExpRate      :: Maybe Rate
    , ratedExpDepends   :: Maybe a
    , ratedExpExp       :: Exp a
    } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data RatedVar = RatedVar 
    { ratedVarRate :: Rate 
    , ratedVarId   :: Int 
    } deriving (Show)

onExp :: (Exp a -> Exp a) -> RatedExp a -> RatedExp a
onExp f a = a{ ratedExpExp = f (ratedExpExp a) }

data VarType = LocalVar | GlobalVar
    deriving (Show, Eq, Ord)

type Exp a = MainExp (PrimOr a)

toPrimOr :: E -> PrimOr E
toPrimOr a = PrimOr $ case ratedExpExp $ unFix a of
    ExpPrim (PString _) -> Right a
    ExpPrim p -> Left p
    _         -> Right a

newtype PrimOr a = PrimOr { unPrimOr :: Either Prim a }
    deriving (Show, Eq, Ord, Functor)

data MainExp a 
    = ExpPrim Prim
    | Tfm Info [a]
    | ConvertRate Rate Rate a
    | Select Rate Int a
    | If (CondInfo a) a a    
    | ExpBool (BoolExp a)
    | ExpNum (NumExp a)
    | ReadVar Var
    | WriteVar Var a    
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)  

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
    | PrimTab (Either Tab LowTab)
    | PrimString String 
    deriving (Show, Eq, Ord)
   
type TabMap = M.Map LowTab Int

data LowTab = LowTab 
    { lowTabSize    :: Int
    , lowTabGen     :: Int
    , lowTabArgs    :: [Double]
    } deriving (Show, Eq, Ord)

-- | Csound f-tables. You can make a value of 'Tab' with the function 'Csound.Tab.gen' or
-- use more higher level functions.
data Tab 
    = TabExp E
    | Tab 
    { tabSize :: TabSize
    , tabGen  :: Int
    , tabArgs :: TabArgs
    } deriving (Show, Eq, Ord)

-- | Table size fidelity (how many points in the table by default).
data TabFi = TabFi
    { tabFiBase   :: Int
    , tabFiGens   :: IM.IntMap Int }

instance Default TabSize where
    def = SizeDegree
        { hasGuardPoint = False
        , sizeDegree = 0 }

data TabSize 
    = SizePlain Int
    | SizeDegree 
    { hasGuardPoint :: Bool
    , sizeDegree    :: Int 
    } deriving (Show, Eq, Ord)
    
data TabArgs 
    = ArgsPlain [Double]
    | ArgsRelative [Double]
    deriving (Show, Eq, Ord)

-- | Midi messages.
data Msg = Msg

type Note = [Prim]

data Event a = Event 
    { eventStart :: Double
    , eventDur   :: Double
    , eventContent :: a }
    
eventEnd e = eventStart e + eventDur e

instance Functor Event where
    fmap f a = a{ eventContent = f $ eventContent a }

------------------------------------------------------------
-- types for arithmetic and boolean expressions

data Inline a b = Inline 
    { inlineExp :: InlineExp a
    , inlineEnv :: IM.IntMap b    
    } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data InlineExp a
    = InlinePrim Int
    | InlineExp a [InlineExp a]
    deriving (Show, Eq, Ord)

data PreInline a b = PreInline a [b]
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

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
    | Ceil | Floor | Frac | Round | IntOp
    | Abs | ExpOp | Log | Log10 | Logbtwo | Sqrt    
    | Ampdb | Ampdbfs | Dbamp | Dbfsamp 
    | Cent | Cpsmidinn | Cpsoct | Cpspch | Octave | Octcps | Octmidinn | Octpch | Pchmidinn | Pchoct | Semitone
    deriving (Show, Eq, Ord)

-------------------------------------------------------
-- instances for cse that ghc was not able to derive for me

instance Foldable PrimOr where foldMap = foldMapDefault

instance Traversable PrimOr where
    traverse f x = case unPrimOr x of
        Left  p -> pure $ PrimOr $ Left p
        Right a -> PrimOr . Right <$> f a

-- comments
-- 
-- p-string 
--
--    separate p-param for strings (we need it to read strings from global table) 
--    Csound doesn't permits us to use more than four string params so we need to
--    keep strings in the global table and use `strget` to read them

