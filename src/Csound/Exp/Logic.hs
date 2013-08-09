{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TypeFamilies #-}
module Csound.Exp.Logic(
    BoolSig(..), BoolD(..), when, whens, 
    ifTuple, caseTuple, guardedTuple,
    ifArg, caseArg, guardedArg
) where

import Control.Arrow(second)
import Control.Monad.Trans.State(State, state, evalState)
import qualified Data.IntMap as IM(fromList)

import Data.Boolean

import Csound.Exp
import Csound.Exp.Wrapper(
    Sig, D, Str,  
    setRate, noRate,
    Val(..), toExp, onExp, onE1)

import Csound.Exp.SE(SE, se_, stmtOnly)
import Csound.Exp.Arg
import Csound.Exp.Tuple

------------------------------------------------------
-- imperative if-then-else

when :: BoolSig -> SE () -> SE ()
when p body = do
    ifBegin p
    body
    ifEnd

whens :: [(BoolSig, SE ())] -> SE () -> SE ()
whens bodies el = case bodies of
    []   -> el
    a:as -> do
        ifBegin (fst a)
        snd a
        elseIfs as
        elseBegin 
        el
        ifEnd
    where elseIfs = mapM_ (\(p, body) -> elseIfBegin p >> body)

ifBegin :: Val a => a -> SE ()
ifBegin = withCond IfBegin

elseIfBegin :: Val a => a -> SE ()
elseIfBegin = withCond ElseIfBegin

elseBegin :: SE ()
elseBegin = stmtOnly ElseBegin

ifEnd :: SE ()
ifEnd = stmtOnly IfEnd

withCond :: Val a => (CondInfo (PrimOr E) -> MainExp (PrimOr E)) -> a -> SE ()
withCond stmt p = se_ $ fromE $ noRate $ stmt (condInfo p)

-- booleans

-- | Boolean signals. 
newtype BoolSig = BoolSig { unBoolSig :: E }

-- | Boolean constants. 
newtype BoolD = BoolD { unBoolD :: E }

instance Val BoolSig    where { toE = unBoolSig;fromE = BoolSig }
instance Val BoolD      where { toE = unBoolD;  fromE = BoolD }

-- booleans for signals

instance Boolean BoolSig where
    true = boolOp0 TrueOp
    false = boolOp0 FalseOp
    notB = onE1 notE
    (&&*) = boolOp2 And
    (||*) = boolOp2 Or

-- instances

type instance BooleanOf Sig = BoolSig

instance IfB Sig where
    ifB = condExp
    
instance EqB Sig where
    (==*) = boolOp2 Equals
    (/=*) = boolOp2 NotEquals
    
instance OrdB Sig where
    (<*) = boolOp2 Less
    (>*) = boolOp2 Greater
    (<=*) = boolOp2 LessEquals
    (>=*) = boolOp2 GreaterEquals

-- boolean tuples

newtype BoolTuple = BoolTuple { unBoolTuple :: [E] }

toBoolTuple :: CsdTuple a => a -> BoolTuple
toBoolTuple   = BoolTuple . fromCsdTuple

fromBoolTuple :: CsdTuple a => BoolTuple -> a
fromBoolTuple = toCsdTuple . unBoolTuple

type instance BooleanOf BoolTuple = BoolSig

instance IfB BoolTuple where
    ifB p (BoolTuple as) (BoolTuple bs) = BoolTuple $ zipWith (condExp p) as bs

-- | @ifB@ for tuples of csound values.
ifTuple :: (CsdTuple a) => BoolSig -> a -> a -> a
ifTuple p a b = fromBoolTuple $ ifB p (toBoolTuple a) (toBoolTuple b)

-- | @guardedB@ for tuples of csound values.
guardedTuple :: (CsdTuple b) => [(BoolSig, b)] -> b -> b
guardedTuple bs b = fromBoolTuple $ guardedB undefined (fmap (second toBoolTuple) bs) (toBoolTuple b)

-- | @caseB@ for tuples of csound values.
caseTuple :: (CsdTuple b) => a -> [(a -> BoolSig, b)] -> b -> b
caseTuple a bs other = fromBoolTuple $ caseB a (fmap (second toBoolTuple) bs) (toBoolTuple other)

-- booleans for inits

i :: Val a => a -> a
i = setRate Ir

instance Boolean BoolD where
    true = boolOp0I TrueOp
    false = boolOp0I FalseOp
    notB = onE1 notE
    (&&*) = boolOp2I And
    (||*) = boolOp2I Or

type instance BooleanOf D = BoolD

instance IfB D where
    ifB = condExpI
    
instance EqB D where
    (==*) = boolOp2I Equals
    (/=*) = boolOp2I NotEquals
    
instance OrdB D where
    (<*) = boolOp2I Less
    (>*) = boolOp2I Greater
    (<=*) = boolOp2I LessEquals
    (>=*) = boolOp2I GreaterEquals

-- booleans for tables

type instance BooleanOf Tab = BoolD

instance IfB Tab where
    ifB = condExpI

-- booleans for strings

type instance BooleanOf Str = BoolD

instance IfB Str where
    ifB = condExpI

newtype BoolArg = BoolArg { unBoolArg :: [E] }

toBoolArg :: (Arg a, CsdTuple a) => a -> BoolArg
toBoolArg   = BoolArg . fromCsdTuple

fromBoolArg :: (Arg a, CsdTuple a) => BoolArg -> a
fromBoolArg = toCsdTuple . unBoolArg

type instance BooleanOf BoolArg = BoolD

instance IfB BoolArg where
    ifB p (BoolArg as) (BoolArg bs) = BoolArg $ zipWith (condExp p) as bs

-- | @ifB@ for constants.
ifArg :: (Arg a, CsdTuple a) => BoolD -> a -> a -> a
ifArg p a b = fromBoolArg $ ifB p (toBoolArg a) (toBoolArg b)

-- | @guardedB@ for constants.
guardedArg :: (CsdTuple b, Arg b) => [(BoolD, b)] -> b -> b
guardedArg bs b = fromBoolArg $ guardedB undefined (fmap (second toBoolArg) bs) (toBoolArg b)

-- | @caseB@ for constants.
caseArg :: (CsdTuple b, Arg b) => a -> [(a -> BoolD, b)] -> b -> b
caseArg a bs other = fromBoolArg $ caseB a (fmap (second toBoolArg) bs) (toBoolArg other)

--------------------------------------------------------------------------
-- if-then-else
--
-- performs inlining of the boolean expressions

boolExp :: a -> [b] -> PreInline a b
boolExp = PreInline

condExpI :: (Val bool, Val a) => bool -> a -> a -> a
condExpI p t e = i $ condExp (i p) (i t) (i e)

condExp :: (Val bool, Val a) => bool -> a -> a -> a
condExp p t e = fromE $ mkCond (condInfo p) (toE t) (toE e)
    where mkCond :: CondInfo (PrimOr E) -> E -> E -> E
          mkCond pr th el 
            | isTrue pr = th
            | isFalse pr = el
            | otherwise = noRate $ If pr (toPrimOr th) (toPrimOr el)            

condInfo :: (Val bool) => bool -> CondInfo (PrimOr E)
condInfo p = go $ toPrimOr $ toE p
    where
        go :: PrimOr E -> CondInfo (PrimOr E)
        go expr = (\(a, b) -> Inline a (IM.fromList b)) $ evalState (condInfo' expr) 0
        condInfo' :: PrimOr E -> State Int (InlineExp CondOp, [(Int, PrimOr E)])
        condInfo' e = maybe (onLeaf e) (onExpr e) $ parseNode e
        onLeaf e = state $ \n -> ((InlinePrim n, [(n, e)]), n+1)  
        onExpr  _ (op, args) = fmap mkNode $ mapM condInfo' args
            where mkNode as = (InlineExp op (map fst as), concat $ map snd as) 

        parseNode :: PrimOr E -> Maybe (CondOp, [PrimOr E])
        parseNode x = case unPrimOr $ fmap toExp x of
          Right (ExpBool (PreInline op args)) -> Just (op, args)
          _ -> Nothing    

--------------------------------------------------------------------------------
-- constructors for boolean expressions

-- generic constructor
boolOps :: (Val a) => CondOp -> [E] -> a
boolOps op as = noRate $ ExpBool $ boolExp op $ fmap toPrimOr as

-- constructors by arity

boolOp0 :: Val a => CondOp -> a
boolOp0 op = boolOps op []

boolOp0I :: Val a => CondOp -> a
boolOp0I op = i $ boolOp0 op

boolOp2 :: (Val a1, Val a2, Val b) => CondOp -> a1 -> a2 -> b
boolOp2 op a b = boolOps op $ map (setRate Kr) [toE a, toE b]

boolOp2I :: (Val a1, Val a2, Val b) => CondOp -> a1 -> a2 -> b
boolOp2I op a b = i $ boolOps op $ map (setRate Ir) [toE a, toE b]

-----------------------------------------------------------------------------
-- no support for not in csound so we perform not-elimination
notE :: E -> E
notE x = onExp phi x
    where phi (ExpBool (PreInline op args)) = ExpBool $ case op of
            TrueOp            -> boolExp FalseOp        []
            FalseOp           -> boolExp TrueOp         []
            And               -> boolExp Or             $ fmap (fmap notE) args
            Or                -> boolExp And            $ fmap (fmap notE) args
            Equals            -> boolExp NotEquals      args
            NotEquals         -> boolExp Equals         args
            Less              -> boolExp GreaterEquals  args
            Greater           -> boolExp LessEquals     args
            LessEquals        -> boolExp Greater        args
            GreaterEquals     -> boolExp Less           args

          phi _ = error "Logic.hs:notE - expression is not Boolean"  


