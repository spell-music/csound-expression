{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
-- | Boolean instances
module Csound.Dynamic.Build.Logic(
    when1, whens,
    ifBegin, ifEnd, elseBegin,
    untilDo,
    untilBegin, untilEnd,
    whileDo,
    whileBegin, whileRef, whileEnd
) where

import Control.Monad.Trans.State(State, state, evalState)
import qualified Data.IntMap as IM(fromList)

import Data.Boolean
import Csound.Dynamic.Types
import Csound.Dynamic.Build(onExp, toExp)

------------------------------------------------------
-- imperative if-then-else

when1 :: Monad m => Rate -> E -> DepT m () -> DepT m ()
when1 rate p body = do
    ifBegin rate p
    body
    ifEnd

whens :: Monad m => Rate -> [(E, DepT m ())] -> DepT m () -> DepT m ()
whens rate bodies el = case bodies of
    []   -> el
    a:as -> do
        ifBegin rate (fst a)
        snd a
        elseIfs as
        elseBegin 
        el
        foldl1 (>>) $ replicate (1 + length as) ifEnd
    where elseIfs = mapM_ (\(p, body) -> elseBegin >> ifBegin rate p >> body)

ifBegin :: Monad m => Rate -> E -> DepT m ()
ifBegin rate = withCond $ IfBegin rate

elseBegin :: Monad m => DepT m ()
elseBegin = stmtOnlyT ElseBegin

ifEnd :: Monad m => DepT m ()
ifEnd = stmtOnlyT IfEnd

untilDo :: Monad m => E -> DepT m () -> DepT m ()
untilDo p body = do
    untilBegin p
    body
    untilEnd

untilBegin :: Monad m => E -> DepT m ()
untilBegin = withCond UntilBegin

untilEnd :: Monad m => DepT m ()
untilEnd = stmtOnlyT UntilEnd

whileDo :: Monad m => E -> DepT m () -> DepT m ()
whileDo p body = do
    whileBegin p
    body
    whileEnd

whileBegin :: Monad m => E -> DepT m ()
whileBegin = withCond WhileBegin

whileRef :: Monad m => Var -> DepT m ()
whileRef var = stmtOnlyT $ WhileRefBegin var

whileEnd :: Monad m => DepT m ()
whileEnd = stmtOnlyT WhileEnd

withCond :: Monad m => (CondInfo (PrimOr E) -> MainExp (PrimOr E)) -> E -> DepT m ()
withCond stmt p = depT_ $ noRate $ stmt (condInfo p)

instance Boolean E where
    true = boolOp0 TrueOp
    false = boolOp0 FalseOp
    notB = notE
    (&&*) = boolOp2 And
    (||*) = boolOp2 Or

-- instances

type instance BooleanOf E = E

instance IfB E where
    ifB = condExp
    
instance EqB E where
    (==*) = boolOp2 Equals
    (/=*) = boolOp2 NotEquals
    
instance OrdB E where
    (<*) = boolOp2 Less
    (>*) = boolOp2 Greater
    (<=*) = boolOp2 LessEquals
    (>=*) = boolOp2 GreaterEquals

--------------------------------------------------------------------------
-- if-then-else
--
-- performs inlining of the boolean expressions

boolExp :: a -> [b] -> PreInline a b
boolExp = PreInline

condExp :: E -> E -> E -> E
condExp = mkCond . condInfo
    where mkCond :: CondInfo (PrimOr E) -> E -> E -> E
          mkCond pr th el 
            | isTrue pr = th
            | isFalse pr = el
            | otherwise = noRate $ If pr (toPrimOr th) (toPrimOr el)            

condInfo :: E -> CondInfo (PrimOr E)
condInfo p = go $ toPrimOr p
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

boolOps :: CondOp -> [E] -> E
boolOps op as = noRate $ ExpBool $ boolExp op $ fmap toPrimOr as

boolOp0 :: CondOp -> E
boolOp2 :: CondOp -> E -> E -> E

boolOp0 op = boolOps op []

boolOp2 op a b = boolOps op [a, b]

fromBoolOpt :: Either Bool E -> E
fromBoolOpt = either (\x -> if x then true else false) id 

toNumOpt :: E -> Either Double E
toNumOpt x = case toExp x of
    ExpPrim (PrimDouble d) -> Left d
    _ -> Right x

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

