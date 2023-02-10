-- | Main types
{-# OPTIONS_GHC -Wno-orphans #-}
{-# Language
        DeriveFunctor, DeriveFoldable, DeriveTraversable,
        DeriveGeneric,
        StandaloneDeriving,
        TypeSynonymInstances, FlexibleInstances,
        TemplateHaskell,
        CPP #-}
module Csound.Dynamic.Types.Exp(
    E, RatedExp(..), isEmptyExp,
    ratedExp, noRate, withRate, setRate, toCtrlRate, toInitRate,
    toArrRate, removeArrRate,
    Exp, toPrimOr, toPrimOrTfm, PrimOr(..), MainExp(..), Name,
    InstrId(..), intInstrId, ratioInstrId, stringInstrId, instrIdRate,
    VarType(..), Var(..), Info(..), OpcFixity(..), Rate(..),
    CodeBlock (..),
    Signature(..), isInfix, isPrefix,
    Prim(..), Gen(..), GenId(..),
    Inline(..), InlineExp(..), PreInline(..),
    BoolExp, CondInfo, CondOp(..), isTrue, isFalse,
    NumExp, NumOp(..), Note,
    MultiOut,
    IsArrInit, ArrSize, ArrIndex,
    IfRate(..), fromIfRate,
    hashE,
    rehashE,
    ExpHash (..),
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Crypto.Hash.SHA256 qualified as Crypto

import GHC.Generics (Generic, Generic1)
import Data.Traversable
import Data.ByteString (ByteString)

import Data.Map.Strict (Map)
import Data.Maybe(isNothing)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntMap.Internal as IM
import Data.Fix
import Data.Eq.Deriving
import Data.Ord.Deriving
import Text.Show.Deriving
import Data.Text (Text)
import Data.Serialize qualified as Cereal
import Data.Serialize.Text ()
import Data.Hashable

type Name = Text
type LineNum = Int

-- | An instrument identifier
data InstrId
    = InstrId
    { instrIdFrac :: !(Maybe Int)
    , instrIdCeil :: !Int }
    | InstrLabel Text
    deriving (Show, Eq, Ord, Generic)

instrIdRate :: InstrId -> Rate
instrIdRate = \case
  InstrId{}    -> Ir
  InstrLabel{} -> Sr

-- | Constructs an instrument id with the integer.
intInstrId :: Int -> InstrId
intInstrId n = InstrId Nothing n

-- | Constructs an instrument id with fractional part.
ratioInstrId :: Int -> Int -> InstrId
ratioInstrId beforeDot afterDot = InstrId (Just $ afterDot) beforeDot

-- | Constructs an instrument id with the string label.
stringInstrId :: Text -> InstrId
stringInstrId = InstrLabel

-- | The inner representation of csound expressions.
type E = Fix RatedExp

data RatedExp a = RatedExp
    { ratedExpHash      :: !ExpHash
       -- ^ expression hash for fast comparison
    , ratedExpRate      :: !(Maybe Rate)
        -- ^ Rate (can be undefined or Nothing,
        -- it means that rate should be deduced automatically from the context)
    , ratedExpDepends   :: !(Maybe (LineNum, a))
        -- ^ Dependency (it is used for expressions with side effects,
        -- value contains the previous statement)
    , ratedExpExp       :: !(Exp a)
        -- ^ Main expression
    } deriving (Show, Functor, Foldable, Traversable, Generic, Generic1)

instance Eq (RatedExp a) where
  (==) a b = ratedExpHash a == ratedExpHash b

instance Ord (RatedExp a) where
  compare a b = ratedExpHash a `compare` ratedExpHash b

ratedExp :: Maybe Rate -> Exp E -> E
ratedExp r expr = Fix $ RatedExp h r Nothing expr
  where
    h = ExpHash $ Crypto.hash $ Cereal.encode $ fmap (fmap hashE) expr

noRate :: Exp E -> E
noRate = ratedExp Nothing

withRate :: Rate -> Exp E -> E
withRate r = ratedExp (Just r)

-- | Hash of the expression for fast comparison
newtype ExpHash = ExpHash { unExpHash :: ByteString }
  deriving newtype (Eq, Show, Ord, Hashable, Cereal.Serialize)

hashE :: E -> ExpHash
hashE (Fix expr) = ratedExpHash expr

-- | Call it on every change in underlying expression
rehashE :: E -> E
rehashE (Fix expr) = Fix $
  expr
    { ratedExpHash = ExpHash $ Crypto.hash $ Cereal.encode $ fmap hashE expr
    }

-- rate coversion

setRate :: Rate -> E -> E
setRate r a =
  case ratedExpExp $ unFix a of
    -- for Tfm we add rate to ratedExpRate hint
    Tfm _ _    -> Fix $ (unFix a) { ratedExpRate = Just r }
    -- conversion set's the rate for constants
    -- ExpPrim _  -> a
    ExpPrim _  -> Fix $ (unFix a) { ratedExpRate = Just r }
    -- don't convert rate twice
    ConvertRate _ b arg -> withRate r $ ConvertRate r b arg
    -- for booleans pass conversion over boolean operators
    ExpBool boolArg -> noRate $ ExpBool $ fmap (fmap (setRate r)) boolArg
    -- for other cases we insert rate conversion
    _          -> withRate r $ ConvertRate r Nothing (PrimOr $ Right a)

-- | It's a primitive value or something else. It's used for inlining
-- of the constants (primitive values).
newtype PrimOr a = PrimOr { unPrimOr :: Either Prim a }
    deriving (Show, Eq, Ord, Functor, Generic, Generic1)

-- | Converts rate to control rate (affects Ir and Ar, other are unchainged)
toCtrlRate :: Rate -> Rate
toCtrlRate = \case
  Ar -> Kr
  Ir -> Kr
  x  -> x

-- | Converts rate to init rate (affects Kr other are unchainged)
toInitRate :: Rate -> Rate
toInitRate = \case
  Kr -> Ir
  Ar -> Ir
  x  -> x

instance Cereal.Serialize a => Cereal.Serialize (PrimOr a)

-- | Constructs PrimOr values from the expressions. It does inlining in
-- case of primitive values.
toPrimOr :: E -> PrimOr E
toPrimOr a = PrimOr $ case ratedExpExp $ unFix a of
    ExpPrim (PString _) -> Right a
    ExpPrim p  -> Left p
    ReadVar v | noDeps -> Left (PrimVar (varRate v) v)
    _         -> Right a
    where
        noDeps = isNothing $ ratedExpDepends $ unFix a

-- | Constructs PrimOr values from the expressions. It does inlining in
-- case of primitive values.
toPrimOrTfm :: Rate -> E -> PrimOr E
toPrimOrTfm r a = PrimOr $ case ratedExpExp $ unFix a of
    ExpPrim (PString _) -> Right a
    ExpPrim p | (r == Ir || r == Sr) -> Left p
    ReadVar v | noDeps -> Left (PrimVar (varRate v) v)
    _         -> Right a
    where
        noDeps = isNothing $ ratedExpDepends $ unFix a


-- Expressions with inlining.
type Exp a = MainExp (PrimOr a)

newtype CodeBlock a = CodeBlock a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1)

-- Csound expressions
data MainExp a
    = EmptyExp
    -- | Primitives
    | ExpPrim !Prim
    -- | Application of the opcode: we have opcode information (Info) and the arguments [a]
    | Tfm Info ![a]
    -- | Rate conversion
    | ConvertRate !Rate !(Maybe Rate) !a
    -- | Selects a cell from the tuple, here argument is always a tuple (result of opcode that returns several outputs)
    | Select !Rate !Int !a
    -- | if-then-else
    | If !IfRate !(CondInfo a) !a !a
    -- | Boolean expressions (rendered in infix notation in the Csound)
    | ExpBool !(BoolExp a)
    -- | Numerical expressions (rendered in infix notation in the Csound)
    | ExpNum !(NumExp a)
    -- | Reading/writing a named variable
    | InitVar !Var !a
    | ReadVar !Var
    | WriteVar !Var !a
    -- | Arrays
    | InitArr !Var !(ArrSize a)
    | ReadArr !Var !(ArrIndex a)
    | WriteArr !Var !(ArrIndex a) !a
    | WriteInitArr !Var !(ArrIndex a) !a
    | TfmArr !IsArrInit !Var !Info ![a]
    -- | inits 1-dimensional read only array (uses fillaray)
    -- args: rateOfTheOutput processingRate initValues
    | InitPureArr !Rate !IfRate ![a]
    -- | Reads read only array with index
    -- args: rateOfTheOutput processingRate array index
    | ReadPureArr !Rate !IfRate !a !a
    -- | Imperative If-then-else
    | IfBlock !IfRate !(CondInfo a) (CodeBlock a)
    | IfElseBlock !IfRate !(CondInfo a) (CodeBlock a) (CodeBlock a)
    | IfBegin !IfRate !(CondInfo a)
    | ElseBegin
    | IfEnd
    -- | looping constructions
    | UntilBlock !IfRate !(CondInfo a) (CodeBlock a)
    | UntilBegin !IfRate !(CondInfo a)
    | UntilEnd
    | WhileBlock !IfRate !(CondInfo a) (CodeBlock a)
    | WhileBegin !IfRate !(CondInfo a)
    | WhileRefBlock !Var !(CodeBlock a)
    | WhileRefBegin !Var
    | WhileEnd
    -- | Verbatim stmt
    | Verbatim !Text
    -- | Dependency tracking
    | Starts
    | Seq a a
    | Ends a
    -- | read macros arguments
    | InitMacrosInt !Text !Int
    | InitMacrosDouble !Text !Double
    | InitMacrosString !Text !Text
    | ReadMacrosInt !Text
    | ReadMacrosDouble !Text
    | ReadMacrosString !Text
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1)

-- | Rate of if-then-else conditional.
-- It can run at Ir or Kr
data IfRate = IfIr | IfKr
  deriving (Show, Eq, Ord, Generic)

fromIfRate :: IfRate -> Rate
fromIfRate = \case
  IfKr -> Kr
  IfIr -> Ir

-- | Can be infinite so fe just ignore the value
instance Cereal.Serialize Signature where
  put = \_a -> pure ()
  get = undefined

instance Cereal.Serialize a => Cereal.Serialize (RatedExp a)
instance Cereal.Serialize Prim
instance Cereal.Serialize Rate
instance Cereal.Serialize IfRate
instance Cereal.Serialize Info
instance Cereal.Serialize OpcFixity
instance Cereal.Serialize InstrId
instance Cereal.Serialize CondOp
instance Cereal.Serialize NumOp
instance Cereal.Serialize Var
instance Cereal.Serialize VarType
instance Cereal.Serialize a => Cereal.Serialize (CodeBlock a)
instance Cereal.Serialize a => Cereal.Serialize (MainExp a)
instance (Cereal.Serialize a, Cereal.Serialize b) => Cereal.Serialize (Inline a b)
instance (Cereal.Serialize a, Cereal.Serialize b) => Cereal.Serialize (PreInline a b)
instance (Cereal.Serialize a) => Cereal.Serialize (InlineExp a)

type IsArrInit = Bool

-- | Array sizes by demensions
type ArrSize a = [a]

-- | Array multi index
type ArrIndex a = [a]

-- Named variable
data Var
    = Var
        { varType :: !VarType    -- global / local
        , varRate :: !Rate
        , varName :: !Name }
    | VarVerbatim
        { varRate :: !Rate
        , varName :: !Name
        } deriving (Show, Eq, Ord, Generic)

-- Variables can be global (then we have to prefix them with `g` in the rendering) or local.
data VarType = LocalVar | GlobalVar
    deriving (Show, Eq, Ord, Generic)

-- Opcode information.
data Info = Info
    -- Opcode name
    { infoName          :: !Name
    -- Opcode type signature
    , infoSignature     :: !Signature
    -- Opcode can be infix or prefix
    , infoOpcFixity     :: !OpcFixity
    } deriving (Show, Eq, Ord, Generic)

isPrefix, isInfix :: Info -> Bool

isPrefix = (Prefix ==) . infoOpcFixity
isInfix  = (Infix  ==) . infoOpcFixity

-- Opcode fixity
data OpcFixity = Prefix | Infix | Opcode
    deriving (Show, Eq, Ord, Generic)

-- | The Csound rates.
data Rate   -- rate:
    ----------------------------
    = Xr    -- audio or control (and I use it for opcodes that produce no output, ie procedures)
    | Ar    -- audio
    | Kr    -- control
    | Ir    -- init (constants)
    | Sr    -- strings
    | Fr    -- spectrum (for pvs opcodes)
    | Wr    -- special spectrum
    | Tvar  -- I don't understand what it is (fix me) used with Fr
    | ArArr -- array rates
    | KrArr
    | IrArr
    | SrArr
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

toArrRate :: Rate -> Rate
toArrRate = \case
  Ar -> ArArr
  Kr -> KrArr
  Ir -> IrArr
  Sr -> SrArr
  other -> other

removeArrRate :: Rate -> Rate
removeArrRate = \case
  ArArr -> Ar
  KrArr -> Kr
  IrArr -> Ir
  SrArr -> Sr
  other -> other

-- Opcode type signature. Opcodes can produce single output (SingleRate) or multiple outputs (MultiRate).
-- In Csound opcodes are often have several signatures. That is one opcode name can produce signals of the
-- different rate (it depends on the type of the outputs). Here we assume (to make things easier) that
-- opcodes that MultiRate-opcodes can produce only the arguments of the same type.
data Signature
    -- For SingleRate-opcodes type signature is the Map from output rate to the rate of the arguments.
    -- With it we can deduce the type of the argument from the type of the output.
    = SingleRate !(Map Rate [Rate])
    -- For MultiRate-opcodes Map degenerates to the singleton. We have only one link.
    -- It contains rates for outputs and inputs.
    | MultiRate
        { outMultiRate :: ![Rate]
        , inMultiRate  :: ![Rate] }
    deriving (Show, Eq, Ord, Generic)

-- Primitive values
data Prim
    -- instrument p-arguments
    = P !Rate !Int
    | PString !Int       -- >> p-string (read p-string notes at the bottom of the file):
    | PrimInt !Int
    | PrimDouble !Double
    | PrimString !Text
    | PrimInstrId !InstrId
    | PrimVar
        { primVarTargetRate :: !Rate
        , primVar           :: !Var }
    deriving (Show, Eq, Ord, Generic)

-- Gen routine.
data Gen = Gen
    { genSize    :: !Int
    , genId      :: !GenId
    , genArgs    :: ![Double]
    , genFile    :: !(Maybe Text)
    } deriving (Show, Eq, Ord, Generic)

data GenId = IntGenId !Int | StringGenId !Text
    deriving (Show, Eq, Ord, Generic)

-- Csound note
type Note = [Prim]

------------------------------------------------------------
-- types for arithmetic and boolean expressions

data Inline op arg = Inline
    { inlineExp :: !(InlineExp op)
    , inlineEnv :: !(IM.IntMap arg)
    } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic1, Generic)

-- Inlined expression.
data InlineExp op
    = InlinePrim !Int
    | InlineExp !op ![InlineExp op]
    deriving (Show, Eq, Ord, Generic)

-- Expression as a tree (to be inlined)
data PreInline a b = PreInline !a ![b]
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1)

-- booleans

type BoolExp a = PreInline CondOp a
type CondInfo a = Inline CondOp a

-- Conditional operators
data CondOp
    = TrueOp | FalseOp | And | Or
    | Equals | NotEquals | Less | Greater | LessEquals | GreaterEquals
    deriving (Show, Eq, Ord, Generic)

isTrue, isFalse :: CondInfo a -> Bool

isTrue  = isCondOp TrueOp
isFalse = isCondOp FalseOp

isCondOp :: CondOp -> CondInfo a -> Bool
isCondOp op = maybe False (op == ) . getCondInfoOp

getCondInfoOp :: CondInfo a -> Maybe CondOp
getCondInfoOp x = case inlineExp x of
    InlineExp op _ -> Just op
    _ -> Nothing

-- Numeric expressions (or Csound infix operators)

type NumExp a = PreInline NumOp a

data NumOp = Add | Sub | Neg | Mul | Div | Pow | Mod
    deriving (Show, Eq, Ord, Generic)

-------------------------------------------------------
-- instances for cse that ghc was not able to derive for me

instance Foldable PrimOr where foldMap = foldMapDefault

instance Traversable PrimOr where
    traverse f x = case unPrimOr x of
        Left  p -> pure $ PrimOr $ Left p
        Right a -> PrimOr . Right <$> f a

----------------------------------------------------------

-- | Multiple output. Specify the number of outputs to get the result.
type MultiOut a = Int -> a

------------------------------------------------------
-- hashable instances

$(deriveEq1 ''PrimOr)
$(deriveEq1 ''PreInline)
$(deriveEq1 ''Inline)
$(deriveEq1 ''CodeBlock)
$(deriveEq1 ''MainExp)
$(deriveEq1 ''RatedExp)

$(deriveOrd1 ''PrimOr)
$(deriveOrd1 ''PreInline)
$(deriveOrd1 ''Inline)
$(deriveOrd1 ''CodeBlock)
$(deriveOrd1 ''MainExp)
$(deriveOrd1 ''RatedExp)

$(deriveShow1 ''PrimOr)
$(deriveShow1 ''PreInline)
$(deriveShow1 ''Inline)
$(deriveShow1 ''CodeBlock)
$(deriveShow1 ''MainExp)
$(deriveShow1 ''RatedExp)

deriving instance Generic1 IM.IntMap

isEmptyExp :: E -> Bool
isEmptyExp (Fix re) = isNothing (ratedExpDepends re) &&
  (case ratedExpExp re of
    EmptyExp -> True
    _ -> False
  )

--------------------------------------------------------------
-- comments
--
-- p-string
--
--    separate p-param for strings (we need it to read strings from global table)
--    Csound doesn't permits us to use more than four string params so we need to
--    keep strings in the global table and use `strget` to read them

