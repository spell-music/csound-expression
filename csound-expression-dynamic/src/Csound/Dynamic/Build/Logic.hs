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
import Control.Monad.Trans.State.Strict (State, state, evalState, runStateT, StateT(..))
import qualified Data.IntMap as IM(fromList)

import Data.Boolean
import Csound.Dynamic.Types
import Csound.Dynamic.Build(onExp, toExp)
import Data.List qualified as List
import Data.Fix

ifT :: forall m . Monad m => IfRate -> E -> DepT m (CodeBlock E) -> DepT m (CodeBlock E) -> DepT m E
ifT ifRate check (DepT th) (DepT el) = DepT $ StateT $ \s -> do
  (_thE, thS) <- runStateT th (startSt s)
  (_elE, elS) <- runStateT el (startSt thS)
  let thDeps = expDependency thS
      elDeps = expDependency elS
      a  = noRate $ IfElseBlock ifRate (condInfo $ setIfRate ifRate check) (CodeBlock $ PrimOr $ Right thDeps) (CodeBlock $ PrimOr $ Right elDeps)
      a1 = rehashE $ Fix $ (unFix a) { ratedExpDepends = Just (newLineNum elS, expDependency elS) }
      s1 = elS
            { newLineNum = succ $ newLineNum elS
            , expDependency = a1
            -- depends (expDependency thS) (depends (expDependency elS) a1)
            }
  pure (a1, s1)
  where
    startSt s = s
      { expDependency = rehashE $ Fix $ (unFix $ noRate Starts) { ratedExpDepends = Just (newLineNum s, noRate Starts) }
      , newLineNum = succ $ newLineNum s
      }


ifT1, untilT, whileT :: Monad m => IfRate -> E -> DepT m (CodeBlock E) -> DepT m E

ifT1 = ifT1By IfBlock
untilT = ifT1By UntilBlock
whileT = ifT1By WhileBlock

ifT1By :: Monad m
  => (IfRate -> CondInfo (PrimOr E) -> CodeBlock (PrimOr E) -> Exp E)
  -> IfRate -> E -> DepT m (CodeBlock E) -> DepT m E
ifT1By cons ifRate check (DepT th) = DepT $ StateT $ \s -> do
  (_thE, thS)  <- runStateT th (startSt s)
  let thDeps = expDependency thS
      a  = noRate $ cons ifRate (condInfo $ setIfRate ifRate check) (CodeBlock $ PrimOr $ Right thDeps)
      a1 = rehashE $ Fix $ (unFix a) { ratedExpDepends = Just (newLineNum thS, expDependency thS) }
      s1 = thS
            { newLineNum = succ $ newLineNum thS
            , expDependency = a1
            -- depends (expDependency thS) (depends (expDependency elS) a1)
            }
  pure (a1, s1)
  where
    startSt s = s
      { expDependency = rehashE $ Fix $ (unFix $ noRate Starts) { ratedExpDepends = Just (newLineNum s, noRate Starts) }
      , newLineNum = succ $ newLineNum s
      }



------------------------------------------------------
-- imperative if-then-else

setIfRate :: IfRate -> E -> E
setIfRate rate = setRate (fromIfRate rate)

when1 :: Monad m => IfRate -> E -> DepT m (CodeBlock E) -> DepT m ()
when1 ifRate p body = void $ ifT1 ifRate p body

whens :: Monad m => IfRate -> [(E, DepT m (CodeBlock E))] -> DepT m (CodeBlock E) -> DepT m ()
whens rate bodies el =
  void $ List.foldl' go el (List.reverse bodies)
  where
    go res (check, th) = CodeBlock <$> ifT rate check th res

ifElseBlock :: Monad m => IfRate -> E -> DepT m (CodeBlock E) -> DepT m (CodeBlock E) -> DepT m ()
ifElseBlock rate p th el = void $ ifElseBlock rate p th el

{-
ifElseBlock' :: Monad m => IfRate -> E -> DepT m (CodeBlock E) -> DepT m (CodeBlock E) -> DepT m (CodeBlock E)
ifElseBlock' ifRate p th el = do
  thE <- th
  elE <- el
  fmap CodeBlock $ depT $ noRate $
    IfElseBlock ifRate
      (condInfo $ setIfRate ifRate p)
      (PrimOr . Right <$> thE)
      (PrimOr . Right <$> elE)
-}
-- withCond ifRate stmt p = depT_ $ noRate $ stmt (condInfo $ setIfRate ifRate p)

{-
ifBegin :: Monad m => IfRate -> E -> DepT m ()
ifBegin ifRate = withCond ifRate $ (IfBegin ifRate)

elseBegin :: Monad m => DepT m ()
elseBegin = stmtOnlyT ElseBegin

ifEnd :: Monad m => DepT m ()
ifEnd = stmtOnlyT IfEnd
-}

untilBlock :: Monad m => IfRate -> E -> DepT m (CodeBlock E) -> DepT m ()
untilBlock ifRate p body = void $ untilT ifRate p body

whileBlock :: Monad m => IfRate -> E -> DepT m (CodeBlock E) -> DepT m ()
whileBlock ifRate p body = void $ whileT ifRate p body

{-
untilDo :: Monad m => IfRate -> E -> DepT m () -> DepT m ()
untilDo ifRate p body = do
    untilBegin ifRate p
    body
    untilEnd

untilBegin :: Monad m => IfRate -> E -> DepT m ()
untilBegin ifRate = withCond ifRate (UntilBegin ifRate)

untilEnd :: Monad m => DepT m ()
untilEnd = stmtOnlyT UntilEnd
-}

{-
whileDo :: Monad m => IfRate -> E -> DepT m () -> DepT m ()
whileDo ifRate p body = do
    whileBegin ifRate p
    body
    whileEnd

whileBegin :: Monad m => IfRate -> E -> DepT m ()
whileBegin ifRate = withCond IfKr (WhileBegin ifRate)
-}

whileRef :: Monad m => Var -> DepT m ()
whileRef var = stmtOnlyT $ WhileRefBegin var

whileEnd :: Monad m => DepT m ()
whileEnd = stmtOnlyT WhileEnd

{-
withCond :: Monad m => IfRate -> (CondInfo (PrimOr E) -> MainExp (PrimOr E)) -> E -> DepT m ()
withCond ifRate stmt p = depT_ $ noRate $ stmt (condInfo $ setIfRate ifRate p)
-}

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
    where mkCond :: CondInfo (PrimOr E) -> E -> E -> E
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

