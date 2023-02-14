{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
-- | Boolean instances
module Csound.Dynamic.Build.Logic(
    when1, whens,
    ifExp,
    ifElseBlock,
    -- ifBegin, ifEnd, elseBegin,
    untilBlock,
    whileBlock,

    -- untilDo,
    -- untilBegin, untilEnd,
    -- whileDo,
    -- whileBegin,
    whileRef, whileEnd
) where

import Control.Monad
import Control.Monad.Trans.State.Strict (State, state, evalState, get, put, execStateT)
import qualified Data.IntMap as IM(fromList)

import Data.Boolean
import Csound.Dynamic.Types
import Csound.Dynamic.Build(onExp, toExp)
import Data.List qualified as List
import Control.Monad.Trans.Class (lift)

ifT :: forall m . Monad m => IfRate -> E -> DepT m () -> DepT m () -> DepT m ()
ifT ifRate check th el = do
  thBlock <- execNoDeps th
  elBlock <- execNoDeps el
  depT_ $ noRate $
    IfElseBlock ifRate (condInfo $ setIfRate ifRate check) (CodeBlock $ PrimOr $ Right thBlock) (CodeBlock $ PrimOr $ Right elBlock)

ifT1, untilT, whileT :: Monad m => IfRate -> E -> DepT m () -> DepT m ()

ifT1 = ifT1By IfBlock
untilT = ifT1By UntilBlock
whileT = ifT1By WhileBlock

execNoDeps :: Monad m => DepT m () -> DepT m E
execNoDeps block = DepT $ do
  st <- get
  st1 <- lift $ execStateT (unDepT block) (st { expDependency = noRate Starts })
  put $ st1 { expDependency = expDependency st }
  pure (expDependency st1)

ifT1By :: Monad m
  => (IfRate -> CondInfo (PrimOr E) -> CodeBlock (PrimOr E) -> Exp E)
  -> IfRate -> E -> DepT m () -> DepT m ()
ifT1By cons ifRate check codeBlock = do
  block <- execNoDeps codeBlock
  depT_ $ noRate $ cons ifRate (condInfo $ setIfRate ifRate check) (CodeBlock $ PrimOr $ Right block)

------------------------------------------------------
-- imperative if-then-else

setIfRate :: IfRate -> E -> E
setIfRate rate = setRate (fromIfRate rate)

when1 :: Monad m => IfRate -> E -> DepT m () -> DepT m ()
when1 ifRate p body = void $ ifT1 ifRate p body

whens :: Monad m => IfRate -> [(E, DepT m ())] -> DepT m () -> DepT m ()
whens rate bodies el =
  void $ List.foldl' go el (List.reverse bodies)
  where
    go res (check, th) = ifT rate check th res

ifElseBlock :: Monad m => IfRate -> E -> DepT m (CodeBlock E) -> DepT m (CodeBlock E) -> DepT m ()
ifElseBlock rate p th el = void $ ifElseBlock rate p th el

untilBlock :: Monad m => IfRate -> E -> DepT m () -> DepT m ()
untilBlock ifRate p body = void $ untilT ifRate p body

whileBlock :: Monad m => IfRate -> E -> DepT m () -> DepT m ()
whileBlock ifRate p body = void $ whileT ifRate p body

whileRef :: Monad m => Var -> DepT m ()
whileRef var = stmtOnlyT $ WhileRefBegin var

whileEnd :: Monad m => DepT m ()
whileEnd = stmtOnlyT WhileEnd

instance Boolean E where
    true = boolOp0 TrueOp
    false = boolOp0 FalseOp
    notB = notE
    (&&*) = boolOp2 And
    (||*) = boolOp2 Or

-- instances

type instance BooleanOf E = E

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

ifExp :: IfRate -> E -> E -> E -> E
ifExp ifRate c = mkCond (condInfo (setIfRate ifRate c))
  where
    mkCond :: CondInfo (PrimOr E) -> E -> E -> E
    mkCond pr th el
      | isTrue pr = th
      | isFalse pr = el
      | otherwise = noRate $ If ifRate pr (toPrimOr th) (toPrimOr el)

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

