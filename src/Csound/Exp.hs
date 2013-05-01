-- | Main types
{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Csound.Exp(
    E, RatedExp(..), RatedVar(..), Exp, toPrimOr, PrimOr(..), MainExp(..), Name,
    VarType(..), Var(..), Info(..), OpcFixity(..), Rate(..), 
    Signature(..), isProcedure, isInfix, isPrefix,    
    Prim(..), LowTab(..), Tab(..), TabSize(..), TabArgs(..), TabMap, TabFi(..),
    Inline(..), InlineExp(..), PreInline(..),
    BoolExp, CondInfo, CondOp(..), isTrue, isFalse,    
    NumExp, NumOp(..), Msg(..), MidiType(..), Note,
    StringMap
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

type Name = String

-- | The inner representation of csound expressions.
type E = Fix RatedExp

data RatedExp a = RatedExp 
    { ratedExpRate      :: Maybe Rate       
        -- ^ Rate (can be undefined or Nothing, 
        -- it means that rate should be deduced automatically from the context)
    , ratedExpDepends   :: Maybe a          
        -- ^ Dependency (it is used for expressions with side effects,
        -- value contains the privious statement)
    , ratedExpExp       :: Exp a
        -- ^ Main expression
    } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | RatedVar is for pretty printing of the wiring ports.
data RatedVar = RatedVar 
    { ratedVarRate :: Rate   
    , ratedVarId   :: Int 
    } deriving (Show)

-- | It's a primitive value or something else. It's used for inlining
-- of the constants (primitive values).
newtype PrimOr a = PrimOr { unPrimOr :: Either Prim a }
    deriving (Show, Eq, Ord, Functor)

-- | Constructs PrimOr values from the expressions. It does inlining in
-- case of primitive values.
toPrimOr :: E -> PrimOr E
toPrimOr a = PrimOr $ case ratedExpExp $ unFix a of
    ExpPrim (PString _) -> Right a
    ExpPrim p -> Left p
    _         -> Right a

-- Expressions with inlining.
type Exp a = MainExp (PrimOr a)

-- Csound expressions
data MainExp a 
    -- | Primitives
    = ExpPrim Prim
    -- | Application of the opcode: we have opcode information (Info) and the arguments [a] 
    | Tfm Info [a]
    -- | Rate conversion
    | ConvertRate Rate Rate a
    -- | Selects a cell from the tuple, here argument is always a tuple (result of opcode that returns several outputs)
    | Select Rate Int a
    -- | if-then-else
    | If (CondInfo a) a a    
    -- | Boolean expressions (rendered in infix notation in the Csound)
    | ExpBool (BoolExp a)
    -- | Numerical expressions (rendered in infix notation in the Csound)
    | ExpNum (NumExp a)
    -- | Reading/writing a named variable
    | ReadVar Var
    | WriteVar Var a    
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)  

-- Named variable
data Var 
    = Var
        { varType :: VarType    -- global / local
        , varRate :: Rate
        , varName :: Name } 
    | VarVerbatim 
        { varRate :: Rate
        , varName :: Name        
        } deriving (Show, Eq, Ord)       
        
-- Variables can be global (then we have to prefix them with `g` in the rendering) or local.
data VarType = LocalVar | GlobalVar
    deriving (Show, Eq, Ord)

-- Opcode information.
data Info = Info 
    -- | Opcode name
    { infoName          :: Name     
    -- | Opcode type signature
    , infoSignature     :: Signature
    -- | Opcode can be infix or prefix
    , infoOpcFixity     :: OpcFixity
    , infoNextSE        :: Maybe Int
    } deriving (Show, Eq, Ord)           
  
isPrefix, isInfix, isProcedure :: Info -> Bool

isPrefix = (Prefix ==) . infoOpcFixity
isInfix  = (Infix  ==) . infoOpcFixity
isProcedure = (Procedure ==) . infoOpcFixity
 
-- Opcode fixity
data OpcFixity = Prefix | Infix | Procedure
    deriving (Show, Eq, Ord)

-- | The Csound rates.
data Rate   -- rate:
    ----------------------------
    = Xr    -- audio or control (and I use it for opcodes that produce no output, ie procedures)
    | Ar    -- audio 
    | Kr    -- control 
    | Ir    -- init (constants)    
    | Sr    -- strings
    | Fr    -- spectrum (for pvs opcodes)
    deriving (Show, Eq, Ord, Enum, Bounded)
    
-- Opcode type signature. Opcodes can produce single output (SingleRate) or multiple outputs (MultiRate).
-- In Csound opcodes are often have several signatures. That is one opcode name can produce signals of the 
-- different rate (it depends on the type of the outputs). Here we assume (to make things easier) that
-- opcodes that MultiRate-opcodes can produce only the arguments of the same type. 
data Signature 
    -- For SingleRate-opcodes type signature is the Map from output rate to the rate of the arguments.
    -- With it we can deduce the type of the argument from the type of the output.
    = SingleRate (Map Rate [Rate]) 
    -- For MultiRate-opcodes Map degenerates to the singleton. We have only one link. 
    -- It contains rates for outputs and inputs.
    | MultiRate 
        { outMultiRate :: [Rate] 
        , inMultiRate  :: [Rate] } 
    deriving (Show, Eq, Ord)

-- Primitive values
data Prim 
    -- instrument p-arguments
    = P Int 
    | PString Int       -- >> p-string: 
    | PrimInt Int 
    | PrimDouble Double 
    | PrimString String 
    -- Here we use Tab and well LowTab. Tab contains no size. It is deduced 
    -- from the renderer settings.
    | PrimTab (Either Tab LowTab)
    deriving (Show, Eq, Ord)

-- Map from tables to unique identifiers.
type TabMap = M.Map LowTab Int

-- Table that has all parameters calculated.
data LowTab = LowTab 
    { lowTabSize    :: Int
    , lowTabGen     :: Int
    , lowTabArgs    :: [Double]
    } deriving (Show, Eq, Ord)

-- Table that can have relative size (to be defined from the renderer settings).

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

-- Table size.
data TabSize 
    -- Size is fixed by the user.
    = SizePlain Int
    -- Size is relative to the renderer settings.
    | SizeDegree 
    { hasGuardPoint :: Bool
    , sizeDegree    :: Int      -- is the power of two
    } deriving (Show, Eq, Ord)
    
-- Table arguments can be
data TabArgs 
    -- absolute
    = ArgsPlain [Double]
    -- or relative to the table size (used for tables that implement interpolation)
    | ArgsRelative [Double]
    deriving (Show, Eq, Ord)

-- | Midi messages.
data Msg = Msg

data MidiType = Massign | Pgmassign (Maybe Int)

-- Csound note
type Note = [Prim]

---------------------------------------------------------
-- Strings 

type StringMap = M.Map String Int

------------------------------------------------------------
-- types for arithmetic and boolean expressions

data Inline a b = Inline 
    { inlineExp :: InlineExp a
    , inlineEnv :: IM.IntMap b    
    } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- Inlined expression. 
data InlineExp a
    = InlinePrim Int
    | InlineExp a [InlineExp a]
    deriving (Show, Eq, Ord)

-- Expression as a tree (to be inlined)
data PreInline a b = PreInline a [b]
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- booleans

type BoolExp a = PreInline CondOp a
type CondInfo a = Inline CondOp a

-- Conditional operators
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

-- Numeric expressions (or Csound infix operators)

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

