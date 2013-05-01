{-# Language TypeFamilies #-}
module Csound.Exp.Logic() where

import Control.Monad.Trans.State(State, state, evalState)
import qualified Data.IntMap as IM(fromList)

import Data.Boolean

import Csound.Exp
import Csound.Exp.Wrapper(
    Sig, D, Str, BoolSig, BoolD, 
    setRate, noRate,
    Val(..), toExp, onExp, onE1)

-- booleans for signals

instance Boolean BoolSig where
    true = boolOp0 TrueOp
    false = boolOp0 FalseOp
    notB = onE1 notE
    (&&*) = boolOp2 And
    (||*) = boolOp2 Or

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

-- booleans for inits

instance Boolean BoolD where
    true = boolOp0 TrueOp
    false = boolOp0 FalseOp
    notB = onE1 notE
    (&&*) = boolOp2 And
    (||*) = boolOp2 Or

type instance BooleanOf D = BoolD

instance IfB D where
    ifB = condExp
    
instance EqB D where
    (==*) = boolOp2 Equals
    (/=*) = boolOp2 NotEquals
    
instance OrdB D where
    (<*) = boolOp2 Less
    (>*) = boolOp2 Greater
    (<=*) = boolOp2 LessEquals
    (>=*) = boolOp2 GreaterEquals

-- booleans for tables

type instance BooleanOf Tab = BoolD

instance IfB Tab where
    ifB = condExp

-- booleans for strings

type instance BooleanOf Str = BoolD

instance IfB Str where
    ifB = condExp

--------------------------------------------------------------------------
-- if-then-else
--
-- performs inlining of the boolean expressions

boolExp = PreInline

condExp :: (Val bool, Val a) => bool -> a -> a -> a
condExp p t e = fromE $ mkCond (condInfo $ toPrimOr $ toE p) (toE t) (toE e)
    where mkCond :: CondInfo (PrimOr E) -> E -> E -> E
          mkCond p t e 
            | isTrue p = t
            | isFalse p = e
            | otherwise = noRate $ If p (toPrimOr t) (toPrimOr e)            

condInfo :: PrimOr E -> CondInfo (PrimOr E)
condInfo exp = (\(a, b) -> Inline a (IM.fromList b)) $ evalState (condInfo' exp) 0
    where condInfo' :: PrimOr E -> State Int (InlineExp CondOp, [(Int, PrimOr E)])
          condInfo' e = maybe (onLeaf e) (onExp e) $ parseNode e
          onLeaf e = state $ \n -> ((InlinePrim n, [(n, e)]), n+1)  
          onExp  e (op, args) = fmap mkNode $ mapM condInfo' args
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

boolOp1 :: Val a => CondOp -> a -> a
boolOp1 op a = boolOps op [setRate Kr $ toE a]

boolOp2 :: (Val a1, Val a2, Val b) => CondOp -> a1 -> a2 -> b
boolOp2 op a b = boolOps op $ map (setRate Kr) [toE a, toE b]

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

