module Csound.Dynamic.Build (

    -- * Expression tree
    -- | Working with expression tree
    toExp, onExp,

    -- * Rates
    -- * Queries
    getRates, isMultiOutSignature, getPrimUnsafe,

    -- * Constructors
    -- | Basic constructors
    prim, opcPrefix, oprPrefix, oprInfix,
    numExp1, numExp2,
    tfm, tfmNoInlineArgs, pn, withInits,
    double, int, str, verbatim, instrIdE,
    inlineVar, gInit, gInitDouble,

    -- ** Opcodes constructors
    Spec1, spec1, opcs, opcsNoInlineArgs, opr1, opr1k, infOpr, oprBy,
    Specs, specs, MultiOut, mopcs, mo,

    -- * Global init statements
    setSr, setKsmps, setNchnls, setNchnls_i, setKr, setZeroDbfs,

    -- * Arrays
    opcsArr, infOprArr
) where

import qualified Data.Map as M(fromList, toList)

import Data.List(transpose)
import Data.Fix(Fix(..))

import Csound.Dynamic.Types.Exp
import Csound.Dynamic.Types.Dep

------------------------------------------------
-- basic constructors

prim :: Prim -> E
prim = noRate . ExpPrim

opcPrefix :: Name -> Signature -> Info
opcPrefix name signature = Info name signature Opcode

oprPrefix :: Name -> Signature -> Info
oprPrefix name signature = Info name signature Prefix

oprInfix :: Name -> Signature -> Info
oprInfix name signature = Info name signature Infix

tfm :: Info -> [E] -> E
tfm info args = noRate $ Tfm info $ zipWith toPrimOrTfm (getInfoRates info) args

tfmArr :: Monad m => IsArrInit -> Var -> Info -> [E] -> DepT m ()
tfmArr isArrInit var info args = depT_ $ noRate $ TfmArr isArrInit var info $ zipWith toPrimOrTfm (getInfoRates info) args

getInfoRates :: Info -> [Rate]
getInfoRates a = getInRates $ infoSignature a
    where
        getInRates x = case x of
            SingleRate m    -> fmap minimum $ transpose $ fmap snd $ M.toList m
            MultiRate _ ins -> ins

tfmNoInlineArgs :: Info -> [E] -> E
tfmNoInlineArgs info args = noRate $ Tfm info $ fmap (PrimOr . Right) args

inlineVar :: Var -> E
inlineVar = Fix . RatedExp Nothing Nothing . ReadVar

pn :: Int -> E
pn = prim . P

withInits :: E -> [E] -> E
withInits a es = onExp phi a
    where phi x = case x of
            -- for opcodes with single output
            Tfm t xs -> Tfm t (xs ++ (fmap toPrimOr es))
            -- for opcodes with multiple outputs
            Select r n expr -> Select r n $ fmap (\t -> withInits t es) expr
            _        -> x

-- | Converts Haskell's doubles to Csound's doubles
double :: Double -> E
double = prim . PrimDouble

-- | Converts Haskell's strings to Csound's strings
str :: String -> E
str = prim . PrimString

-- | Converts Haskell's integers to Csound's doubles
int :: Int -> E
int = prim . PrimInt

verbatim :: Monad m => String -> DepT m ()
verbatim = stmtOnlyT . Verbatim

instrIdE :: InstrId -> E
instrIdE x = case x of
    InstrId Nothing  m -> int m
    InstrId (Just _) _ -> error "instrId undefined for fractional InstrIds"
    InstrLabel s -> str s
----------------------------------------------------------------------
-- constructing opcodes

-- single output

-- User friendly type for single output type signatures
type Spec1 = [(Rate, [Rate])]

spec1 :: Spec1 -> Signature
spec1 = SingleRate . M.fromList

opcs :: Name -> Spec1 -> [E] -> E
opcs name signature = tfm (opcPrefix name $ spec1 signature)

opcsNoInlineArgs :: Name -> Spec1 -> [E] -> E
opcsNoInlineArgs name signature = tfmNoInlineArgs (opcPrefix name $ spec1 signature)

opr1 :: Name -> E -> E
opr1 name a = tfm (oprPrefix name $ spec1 [(Ar, [Ar]), (Kr, [Kr]), (Ir, [Ir])]) [a]

oprBy :: Name -> Spec1 -> [E] -> E
oprBy name signature = tfm (oprPrefix name $ spec1 signature)

opr1k :: Name -> E -> E
opr1k name a = tfm (oprPrefix name $ spec1 [(Kr, [Kr]), (Ir, [Ir])]) [a]

infOpr :: Name -> E -> E -> E
infOpr name a b = tfm (oprInfix name $ spec1 [(Ar, [Ar, Ar]), (Kr, [Kr, Kr]), (Ir, [Ir, Ir])]) [a, b]

numExp1 :: NumOp -> E -> E
numExp1 op x = noRate $ ExpNum $ fmap toPrimOr $ PreInline op [x]

numExp2 :: NumOp -> E -> E -> E
numExp2 op a b = noRate $ ExpNum $ fmap toPrimOr $ PreInline op [a, b]

opcsArr :: Monad m => IsArrInit -> Var -> Name -> Spec1 -> [E] -> DepT m ()
opcsArr isArrInit out name signature = tfmArr isArrInit out (opcPrefix name $ spec1 signature)

infOprArr :: Monad m => IsArrInit -> Var -> Name -> E -> E -> DepT m ()
infOprArr isArrInit out name a b = tfmArr isArrInit out (oprInfix name $ spec1 [(Ar, [Ar, Ar]), (Kr, [Kr, Kr]), (Ir, [Ir, Ir])]) [a, b]

-- multiple output

-- User friendly type for multiple outputs type signatures
type Specs = ([Rate], [Rate])

specs :: Specs -> Signature
specs = uncurry MultiRate

mopcs :: Name -> Specs -> [E] -> MultiOut [E]
mopcs name signature as = \numOfOuts -> mo numOfOuts $ tfm (opcPrefix name $ specs signature) as

mo :: Int -> E -> [E]
mo n e = zipWith (\cellId r -> select cellId r e') [0 ..] outRates
    where outRates = take n $ getRates $ toExp e
          e' = onExp (setMultiRate outRates) e

          setMultiRate rates (Tfm info xs) = Tfm (info{ infoSignature = MultiRate rates ins }) xs
              where ins = case infoSignature info of
                        MultiRate _ a -> a
                        _ -> error "Tuple.hs: multiOutsSection -- should be multiOut expression"
          setMultiRate _ _ = error "Tuple.hs: multiOutsSection -- argument should be Tfm-expression"

          select cellId rate expr = withRate rate $ Select rate cellId (PrimOr $ Right expr)


getRates :: MainExp a -> [Rate]
getRates (Tfm info _) = case infoSignature info of
    MultiRate outs _ -> outs
    _ -> error "Build.hs:getRates - argument should be multiOut"
getRates _ = error "Build.hs:getRates - argument should be Tfm-expression"


isMultiOutSignature :: Signature -> Bool
isMultiOutSignature x = case x of
    MultiRate _ _ -> True
    _ -> False

getPrimUnsafe :: E -> Prim
getPrimUnsafe x = case toExp x of
    ExpPrim a   -> a
    _           -> error "Csound.Dynamic.Build.getPrimUnsafe:Expression is not a primitive"

-- expression tree

toExp :: E -> Exp E
toExp = ratedExpExp . unFix

-- Lifts transformation of main expression
onExp :: (Exp E -> Exp E) -> E -> E
onExp f x = case unFix x of
    a -> Fix $ a{ ratedExpExp = f (ratedExpExp a) }

----------------------------------------------------------------
-- global inits

setSr, setKsmps, setNchnls, setNchnls_i, setKr :: Monad m => Int -> DepT m ()

setZeroDbfs :: Monad m => Double -> DepT m  ()

setGlobal :: (Monad m, Show a) => String -> a -> DepT m  ()
setGlobal name val = verbatim $ name ++ " = " ++ show val

setSr       = setGlobal "sr"
setKr       = setGlobal "kr"
setNchnls   = setGlobal "nchnls"
setNchnls_i = setGlobal "nchnls_i"
setKsmps    = setGlobal "ksmps"
setZeroDbfs = setGlobal "0dbfs"

gInit :: Monad m => String -> Int -> DepT m ()
gInit name val = writeVar (VarVerbatim Ir name) (int val)

gInitDouble :: Monad m => String -> Double -> DepT m ()
gInitDouble name val = writeVar (VarVerbatim Ir name) (double val)

