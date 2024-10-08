module Csound.Dynamic.Build (
  -- * Expression tree

  -- | Working with expression tree
  toExp,
  onExp,

  -- * Rates

  -- * Queries
  getRates,
  isMultiOutSignature,
  getPrimUnsafe,

  -- * Constructors

  -- | Basic constructors
  emptyE,
  prim,
  opcPrefix,
  oprPrefix,
  oprInfix,
  numExp1,
  tfm,
  tfmNoInlineArgs,
  pn,
  withInits,
  double,
  int,
  str,
  verbatim,
  instrIdE,
  inlineVar,
  gInit,
  gInitDouble,

  -- ** Opcodes constructors
  Spec1,
  spec1,
  opcs,
  opcsDep,
  opcsDep_,
  opcsNoInlineArgs,
  opr1,
  opr1k,
  opr1kDep,
  infOpr,
  infOprDep,
  oprBy,
  oprByDep,
  Specs,
  specs,
  MultiOut,
  mopcs,
  mopcsDep,
  mo,

  -- * Global init statements
  setSr,
  setKsmps,
  setNchnls,
  setNchnls_i,
  setKr,
  setZeroDbfs,

  -- * Arrays
  opcsArr,
  infOprArr,
  initPureArr,
  readPureArr,
) where

import Data.Fix (Fix (..))
import Data.Map qualified as M (fromList)
import Data.Serialize qualified as Cereal

import Csound.Dynamic.Types.Dep
import Csound.Dynamic.Types.Exp
import Data.Text (Text)
import Data.Text qualified as Text

------------------------------------------------
-- basic constructors

emptyE :: E
emptyE = noRate EmptyExp

prim :: Prim -> E
prim = noRate . ExpPrim

opcPrefix :: Name -> Signature -> Info
opcPrefix name signature = Info name signature Opcode

oprPrefix :: Name -> Signature -> Info
oprPrefix name signature = Info name signature Prefix

oprInfix :: Name -> Signature -> Info
oprInfix name signature = Info name signature Infix

toArgs :: [Rate] -> [E] -> [PrimOr E]
toArgs = zipWith toPrimOrTfm

toArgsNoInlineConst :: [Rate] -> [E] -> [PrimOr E]
toArgsNoInlineConst = zipWith toPrimOrTfmNoConst

tfm :: Info -> [E] -> E
tfm info args = noRate $ Tfm info $ toArgs (getInfoRates info) args

tfmArr :: (Monad m) => IsArrInit -> Var -> Info -> [E] -> DepT m ()
tfmArr isArrInit var info args =
  depT_ $ noRate $ TfmArr isArrInit var info $ toArgs (getInfoRates info) args

tfmNoInlineArgs :: Info -> [E] -> E
tfmNoInlineArgs info args =
  noRate $ Tfm info $ toArgsNoInlineConst (getInfoRates info) args

inlineVar :: IfRate -> Var -> E
inlineVar ifRate var = Fix $ RatedExp h Nothing Nothing $ ReadVar ifRate var
  where
    h = ExpHash (Cereal.encode var)

pn :: Rate -> Int -> E
pn rate = prim . P rate

withInits :: E -> [E] -> E
withInits a es = onExp phi a
  where
    phi = \case
      -- for opcodes with single output
      Tfm t xs -> Tfm t (xs ++ (fmap toPrimOr es))
      -- for opcodes with multiple outputs
      Select r n expr -> Select r n $ fmap (\t -> withInits t es) expr
      x -> x

-- | Converts Haskell's doubles to Csound's doubles
double :: Double -> E
double = prim . PrimDouble

-- | Converts Haskell's strings to Csound's strings
str :: String -> E
str = prim . PrimString . Text.pack

-- | Converts Haskell's integers to Csound's doubles
int :: Int -> E
int = prim . PrimInt

verbatim :: (Monad m) => Text -> DepT m ()
verbatim = stmtOnlyT . Verbatim

instrIdE :: InstrId -> E
instrIdE x =
  case x of
    InstrId Nothing m -> int m
    InstrId (Just _) _ -> error "instrId undefined for fractional InstrIds"
    InstrLabel s -> prim (PrimString s)

----------------------------------------------------------------------
-- constructing opcodes

-- single output

-- User friendly type for single output type signatures
type Spec1 = [(Rate, [Rate])]

spec1 :: Spec1 -> Signature
spec1 = SingleRate . M.fromList

opcs :: Name -> Spec1 -> [E] -> E
opcs name signature = tfm (opcPrefix name $ spec1 signature)

opcsDep :: (Monad m) => Name -> Spec1 -> [E] -> DepT m E
opcsDep name signature = tfmDep (opcPrefix name $ spec1 signature)

opcsDep_ :: (Monad m) => Name -> Spec1 -> [E] -> DepT m ()
opcsDep_ name signature args = depT_ (tfm (opcPrefix name $ spec1 signature) args)

opcsNoInlineArgs :: Name -> Spec1 -> [E] -> E
opcsNoInlineArgs name signature = tfmNoInlineArgs (opcPrefix name $ spec1 signature)

opr1 :: Name -> E -> E
opr1 name a = tfm (oprPrefix name $ spec1 [(Ar, [Ar]), (Kr, [Kr]), (Ir, [Ir])]) [a]

oprBy :: Name -> Spec1 -> [E] -> E
oprBy name signature = tfm (oprPrefix name $ spec1 signature)

oprByDep :: (Monad m) => Name -> Spec1 -> [E] -> DepT m E
oprByDep name signature = tfmDep (oprPrefix name $ spec1 signature)

opr1k :: Name -> E -> E
opr1k name a = tfm (oprPrefix name $ spec1 [(Kr, [Kr]), (Ir, [Ir])]) [a]

opr1kDep :: (Monad m) => Name -> E -> DepT m E
opr1kDep name a = tfmDep (oprPrefix name $ spec1 [(Kr, [Kr]), (Ir, [Ir])]) [a]

infOpr :: Name -> E -> E -> E
infOpr name a b = tfm (oprInfix name $ spec1 [(Ar, [Ar, Ar]), (Kr, [Kr, Kr]), (Ir, [Ir, Ir])]) [a, b]

infOprDep :: (Monad m) => Name -> E -> E -> DepT m E
infOprDep name a b = tfmDep (oprInfix name $ spec1 [(Ar, [Ar, Ar]), (Kr, [Kr, Kr]), (Ir, [Ir, Ir])]) [a, b]

numExp1 :: NumOp -> E -> E
numExp1 op x = noRate $ ExpNum $ fmap toPrimOr $ PreInline op [x]

opcsArr :: (Monad m) => IsArrInit -> Var -> Name -> Spec1 -> [E] -> DepT m ()
opcsArr isArrInit out name signature = tfmArr isArrInit out (opcPrefix name $ spec1 signature)

infOprArr :: (Monad m) => IsArrInit -> Var -> Name -> E -> E -> DepT m ()
infOprArr isArrInit out name a b = tfmArr isArrInit out (oprInfix name $ spec1 [(Ar, [Ar, Ar]), (Kr, [Kr, Kr]), (Ir, [Ir, Ir])]) [a, b]

initPureArr :: Rate -> IfRate -> [E] -> E
initPureArr outRate procRate initVals =
  noRate $ InitPureArr outRate procRate $ toArgs (repeat initRate) initVals
  where
    initRate = fromIfRate procRate

readPureArr :: Rate -> IfRate -> E -> E -> E
readPureArr outRate procRate arr index =
  noRate $ ReadPureArr outRate procRate (toPrimOr arr) (toPrimOrTfm (fromIfRate procRate) index)

-- multiple output

-- User friendly type for multiple outputs type signatures
type Specs = ([Rate], [Rate])

specs :: Specs -> Signature
specs = uncurry MultiRate

mopcs :: Name -> Specs -> [E] -> MultiOut [E]
mopcs name signature as =
  \numOfOuts ->
    mo
      ( take numOfOuts $ fst signature
      )
      $ tfm (opcPrefix name $ limitMultiRateSignature numOfOuts signature) as

limitMultiRateSignature :: Int -> Specs -> Signature
limitMultiRateSignature size (outRates, inRates) =
  MultiRate (take size outRates) inRates

mopcsDep :: (Monad m) => Int -> Name -> Specs -> [E] -> DepT m [E]
mopcsDep numOfOuts name signature as =
  moDep rates <$> tfmDepVar (opcPrefix name (limitMultiRateSignature numOfOuts signature)) as
  where
    rates = take numOfOuts $ fst signature

mo :: [Rate] -> E -> [E]
mo outRates e = zipWith (\cellId r -> select cellId r e) [0 ..] outRates
  where
    select cellId rate expr = withRate rate $ Select rate cellId (PrimOr $ Right expr)

moDep :: [Rate] -> TmpVar -> [E]
moDep outRates e = zipWith (\cellId r -> select cellId r e) [0 ..] outRates
  where
    select cellId rate tmpVar =
      withRate rate $ Select rate cellId (PrimOr $ Left (PrimTmpVar tmpVar))

getRates :: (Show a) => MainExp a -> [Rate]
getRates = \case
  Tfm info _ -> fromInfo info
  ExpPrim (PrimTmpVar v) ->
    case tmpVarInfo v of
      Just info -> fromInfo info
      Nothing -> error "Build.hs:getRates - no info for tmpVar"
  other -> error $ "Build.hs:getRates - argument should be Tfm-expression. But got " <> show other
  where
    fromInfo info =
      case infoSignature info of
        MultiRate outs _ -> outs
        _ -> error "Build.hs:getRates - argument should be multiOut"

isMultiOutSignature :: Signature -> Bool
isMultiOutSignature x =
  case x of
    MultiRate _ _ -> True
    _ -> False

getPrimUnsafe :: E -> Prim
getPrimUnsafe x = case toExp x of
  ExpPrim a -> a
  _ -> error "Csound.Dynamic.Build.getPrimUnsafe:Expression is not a primitive"

-- expression tree

toExp :: E -> Exp E
toExp = ratedExpExp . unFix

-- Lifts transformation of main expression
onExp :: (Exp E -> Exp E) -> E -> E
onExp f x = rehashE $
  case unFix x of
    a -> Fix $ a{ratedExpExp = f (ratedExpExp a)}

----------------------------------------------------------------
-- global inits

setSr, setKsmps, setNchnls, setNchnls_i, setKr :: (Monad m) => Int -> DepT m ()
setZeroDbfs :: (Monad m) => Double -> DepT m ()
setGlobal :: (Monad m, Show a) => Text -> a -> DepT m ()
setGlobal name val = verbatim $ Text.unwords [name, "=", Text.pack $ show val]
setSr = setGlobal "sr"
setKr = setGlobal "kr"
setNchnls = setGlobal "nchnls"
setNchnls_i = setGlobal "nchnls_i"
setKsmps = setGlobal "ksmps"
setZeroDbfs = setGlobal "0dbfs"

gInit :: (Monad m) => Text -> Int -> DepT m ()
gInit name val = writeVar IfIr (VarVerbatim Ir name) (int val)

gInitDouble :: (Monad m) => Text -> Double -> DepT m ()
gInitDouble name val = writeVar IfIr (VarVerbatim Ir name) (double val)
