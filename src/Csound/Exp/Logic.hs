{-# Language TypeFamilies #-}
module Csound.Exp.Logic() where

import Control.Monad.Trans.State
import Data.Fix
import qualified Data.IntMap as IM
import Control.Applicative

import Data.Boolean

import Csound.Exp.Wrapper
import Csound.Exp
import Csound.Exp.BoolExp
import Csound.Exp.Inline

instance Boolean BoolSig where
    true = boolOp0 TrueOp
    false = boolOp0 FalseOp
    notB = BoolSig . notE . unBoolSig
    (&&*) = boolOp2 And
    (||*) = boolOp2 Or

type instance BooleanOf Sig = BoolSig

instance IfB Sig where
    ifB = cond'
    
instance EqB Sig where
    (==*) = boolOp2 Equals
    (/=*) = boolOp2 NotEquals
    
instance OrdB Sig where
    (<*) = boolOp2 Less
    (>*) = boolOp2 Greater
    (<=*) = boolOp2 LessEquals
    (>=*) = boolOp2 GreaterEquals

--------------------------------------------
-- if-then-else

boolExp = PreInline

cond' :: BoolSig -> Sig -> Sig -> Sig
cond' p t e = wrap $ mkCond (condInfo $ Fix $ unwrap p) (unwrap t) (unwrap e)
    where mkCond :: CondInfo E -> RatedExp E -> RatedExp E -> RatedExp E
          mkCond p t e 
            | isTrue p = t
            | isFalse p = e
            | otherwise = noRate $ If p (Fix t) (Fix e)            

condInfo :: E -> CondInfo E
condInfo exp = (\(a, b) -> Inline a (IM.fromList b)) $ evalState (condInfo' exp) 0
    where condInfo' :: E -> State Int (InlineExp CondOp, [(Int, E)])
          condInfo' e = maybe (onLeaf e) (onExp e) $ parseNode e
          onLeaf e = state $ \n -> ((InlinePrim n, [(n, e)]), n+1)  
          onExp  e (op, args) = mkNode <$> mapM condInfo' args
              where mkNode as = (InlineExp op (map fst as), concat $ map snd as) 

          parseNode :: E -> Maybe (CondOp, [E])
          parseNode x = case ratedExpExp $ unFix x of
              ExpBool (PreInline op args) -> Just (op, args)
              _ -> Nothing    


boolOps :: (Val a) => CondOp -> [E] -> a
boolOps op as = noRate $ ExpBool $ boolExp op as

boolOp0 :: Val a => CondOp -> a
boolOp0 op = boolOps op []

boolOp1 :: Val a => CondOp -> a -> a
boolOp1 op a = boolOps op [setRate Kr $ Fix $ unwrap a]

boolOp2 :: (Val a1, Val a2, Val b) => CondOp -> a1 -> a2 -> b
boolOp2 op a b = boolOps op $ map (Fix . setRate Kr) [unwrap a, unwrap b]

-- no support for not in csound so we perform not-elimination
notE :: E -> E
notE x = Fix $ onExp phi $ unFix x
    where phi (ExpBool (PreInline op args)) = ExpBool $ case op of
            TrueOp            -> boolExp FalseOp        []
            FalseOp           -> boolExp TrueOp         []
            And               -> boolExp Or             $ map notE args
            Or                -> boolExp And            $ map notE args
            Equals            -> boolExp NotEquals      args
            NotEquals         -> boolExp Equals         args
            Less              -> boolExp GreaterEquals  args
            Greater           -> boolExp LessEquals     args
            LessEquals        -> boolExp Greater        args
            GreaterEquals     -> boolExp Less           args
           

