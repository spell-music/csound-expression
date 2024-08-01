{-| We collect all if-blocks under the if-then-else expressions and statements.

For a given if-block of code the taks is to agregate all expressions
that can be used inside that block and don't affect external expressions
relative to that block

For exampe consider expression:

> k3 opcA k2 k1
> k4 opcB 1, 120
>
> if cond then
>   k5 = k3
> else
>   k5 = k4
> endif
>
It can be transformed to:

> if cond then
>  k3 opcA k2 k1
>  k5 = k3
> else
>   k4 opcB 1, 120
>   k5 = k4
> endif

We bring relevant to if-blocks expressions inside the block.
But we should be careful not to touch the expressions that are dependencies
to expressions outside of the block.

The algorithm to find groups of such expressions proceeds as follows:

* count how many times given expression is used in RHS of the expression.
   Create a table for fast access (O (expr-size)). Let's call it global count.

* for a given expression definition start to follow it's dependencies recursively
   and count for all siblings how many times they are used in RHS of the expression.--
   Let's call it local count

* The rule: for a given integer label/name
     * if the global count equals to the local count
         it can be brought inside if-block. Because all it's usages are inside the sub-expressions
         of that block and does not leak to the outer scope.
     * if name is not a sibling of the node for which the rule does not hold true

 There are cases when node is inside if sub-graph but the problem is that one of it's
   parents may be not fit to the graph. To solve this problem we go over the sub-graph 2 times:

   1) to collect local counts we create IntMap of Usage counts local to the if-block
   2) to mark as False all nodes that are not local to if and also (IMPORTANT) mark as False all it's children.
       As we traverse the graph in breadth first we will recursively mark all non fit siblings.
       I hope that it works :)
       On this stage we create a set of nodes which are truly local
   this is a set of local variables

   One buggy solution was to traverse the sub graph and put inside the set the
    nodes which are local regarding the ussage count. But this does not work as
    valid node can have invalid parent. And algorithm will exclude parent but
    keep the child which will lead to the broken code.

This rule works for generic expressions defined on traversable functor F.

But there are some Csound peculiriaties:

* reminder:
     * if-blocks can work on Ir and on Kr rates.
     * Kr if-blocks are ignored on initialization Ir stage.

* this leads to csound syntax specific rules:

   * init expressions can not be brought inside Kr if-block (they will be ignored)
      also Opcodes that run at I-rate.

   * variable / array initialisation can not be brought inside Kr if-block

   * all constants inside the block should have the same rate as the block itself.
      i.e. ir constants inside Ir block and kr constants inside kr block

 So we should recursively follow the depndencies of the if-block root variable definition.
 But we also exclude nodes early if they can not be present inside the block by rate.
-}
module Csound.Dynamic.Tfm.IfBlocks (
  collectIfBlocks,
) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Csound.Dynamic.Tfm.InferTypes (InferenceResult (..), Stmt (..), Var (..))
import Csound.Dynamic.Types.Exp hiding (Var (..))
import Csound.Dynamic.Types.Exp qualified as Exp
import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.STRef
import Data.Text qualified as Text
import Data.Vector.Mutable qualified as Vector
import Data.Vector.Unboxed.Mutable qualified as UnboxedVector

-- import Debug.Trace

type Expr = Stmt Var

collectIfBlocks :: InferenceResult -> InferenceResult
collectIfBlocks infRes@InferenceResult{..}
  | programHasIfs = runST $ do
      env <- newEnv programLastFreshId typedProgram
      uncurry toResult =<< runStateT (collectIter [] $ List.reverse typedProgram) env
  | otherwise = infRes
  where
    toResult :: [Stmt Var] -> Env s -> ST s InferenceResult
    toResult prog Env{..} = do
      lastId <- readSTRef envLastFreshId
      pure $ infRes{typedProgram = prog, programLastFreshId = lastId}

-- | Monad of the algorithm
type Collect s a = StateT (Env s) (ST s) a

type UsageCounts s = UnboxedVector.STVector s Int
type DagGraph s = Vector.STVector s (RatedExp Var)
type IsInits s = UnboxedVector.STVector s Bool

-- | Internal mutable state of the algorithm
data Env s = Env
  { envUsageCount :: UsageCounts s
  , envDag :: DagGraph s
  , envIsInit :: IsInits s
  , envLastFreshId :: STRef s Int
  , envDagSize :: Int
  }

---------------------------------------------------
-- collect interface

getDagSize :: Collect s Int
getDagSize = gets envDagSize

readGlobalUsages :: Int -> Collect s Int
readGlobalUsages n = do
  dagSize <- getDagSize
  if n < dagSize
    then do
      usages <- gets envUsageCount
      lift $ UnboxedVector.read usages n
    else pure 0

readIsInit :: Int -> Collect s Bool
readIsInit n = do
  dagSize <- getDagSize
  if n < dagSize
    then do
      inits <- gets envIsInit
      lift $ UnboxedVector.read inits n
    else pure False

readDag :: Var -> Collect s (Maybe Expr)
readDag lhs = do
  dagSize <- getDagSize
  if varId lhs < dagSize
    then do
      dag <- gets envDag
      fmap (Just . (Stmt lhs)) $ lift $ Vector.read dag (varId lhs)
    else pure Nothing

withDag :: Var -> (Expr -> Collect s ()) -> Collect s ()
withDag n cont = do
  mExpr <- readDag n
  forM_ mExpr cont

freshId :: Collect s Int
freshId = do
  ref <- gets envLastFreshId
  lift $ do
    newId <- readSTRef ref
    modifySTRef' ref succ
    pure newId

---------------------------------------------------------------------------
-- working with DAG-graph

traverseAccumDag ::
  forall s a.
  (Expr -> a -> Collect s a) ->
  a ->
  (Expr -> Collect s Bool) ->
  PrimOr Var ->
  Collect s a
traverseAccumDag update initSt getIsEnd (PrimOr root) = do
  case root of
    Left _ -> pure initSt
    Right var -> do
      ref <- lift $ newSTRef initSt
      visitedRef <- lift $ newSTRef IntSet.empty
      traverseDag visitedRef var getIsEnd (go ref)
      lift $ readSTRef ref
  where
    go :: STRef s a -> Expr -> Collect s ()
    go ref expr = do
      val <- lift $ readSTRef ref
      newVal <- update expr val
      lift $
        writeSTRef ref $
          --       trace (unlines ["GO", show $ stmtLhs expr, show $ ratedExpExp $ stmtRhs expr, show newVal]) $
          newVal

-- | Breadth first traversal
traverseDag :: STRef s IntSet -> Var -> (Expr -> Collect s Bool) -> (Expr -> Collect s ()) -> Collect s ()
traverseDag visitedRef root getIsEnd go = do
  visited <- lift $ readSTRef visitedRef
  unless (IntSet.member (varId root) visited) $ do
    lift $ modifySTRef' visitedRef (IntSet.insert (varId root))
    withDag root $ \expr -> do
      isTerminal <- getIsEnd expr
      unless isTerminal $ do
        go expr
        mapM_ (\var -> traverseDag visitedRef var getIsEnd go) (stmtRhs expr)

-----------------------------------------------------------

newEnv :: forall s. Int -> [Expr] -> ST s (Env s)
newEnv exprSize exprs = do
  usageCount <- UnboxedVector.replicate exprSize 0
  dag <- Vector.new exprSize
  isInit <- UnboxedVector.replicate exprSize False
  exprSizeRef <- newSTRef exprSize
  let
    env = Env usageCount dag isInit exprSizeRef exprSize
  mapM_ (go env) exprs
  pure env
  where
    go :: Env s -> Expr -> ST s ()
    go env expr = do
      updateUsageCount (envUsageCount env) expr
      updateDag (envDag env) expr
      updateIsInit (envIsInit env) expr

    updateUsageCount :: UsageCounts s -> Expr -> ST s ()
    updateUsageCount usageCounts expr =
      mapM_ count (stmtRhs expr)
      where
        count v = UnboxedVector.modify usageCounts succ (varId v)

    updateDag :: DagGraph s -> Expr -> ST s ()
    updateDag dag (Stmt lhs rhs) =
      Vector.write dag (varId lhs) rhs

    updateIsInit :: IsInits s -> Expr -> ST s ()
    updateIsInit isInit expr =
      when (isInitExpr expr) $
        UnboxedVector.write isInit (varId $ stmtLhs expr) True

-- | Be sure not to bring initialization expression inside the if-blocks
isInitExpr :: Stmt Var -> Bool
isInitExpr expr =
  (varType (stmtLhs expr) == Ir) || checkExpr (ratedExpExp $ stmtRhs expr)
  where
    checkExpr = \case
      InitVar _ _ -> True
      InitArr _ _ -> True
      TfmArr isInit _ _ _ -> isInit
      InitPureArr _ _ _ -> True
      InitMacrosInt _ _ -> True
      InitMacrosDouble _ _ -> True
      InitMacrosString _ _ -> True
      ConvertRate Ir _ _ -> True
      Select Ir _ _ -> True
      _ -> False

data ExprType a
  = PlainType
  | IfType IfRate (CondInfo a) a (IfCons a)
  | IfElseType IfRate (CondInfo a) a a (IfElseCons a)
  | IfExpType IfRate (CondInfo a) a a

data IfCons a = IfCons
  { ifBegin :: IfRate -> CondInfo a -> MainExp a
  , ifEnd :: MainExp a
  }

data IfElseCons a = IfElseCons
  { ifElseBegin :: IfRate -> CondInfo a -> MainExp a
  , elseBegin :: MainExp a
  , ifElseEnd :: MainExp a
  }

type LocalUsageCounts = IntMap Int
type LocalVars = IntSet

{-| We process statements in reverse order
and then also accumulation happens in reverse
so we don't need to reverse twice
-}
collectIter :: [Stmt Var] -> [Stmt Var] -> Collect s [Stmt Var]
collectIter results = \case
  [] -> pure results
  expr : exprs ->
    case getExprType (stmtRhs expr) of
      PlainType -> onPlain expr exprs
      IfType rate check th cons -> onIf rate check th cons (stmtLhs expr) exprs
      IfElseType rate check th el cons -> onIfElse rate check th el cons (stmtLhs expr) exprs
      IfExpType rate check th el -> onIfExp rate check th el (stmtLhs expr) exprs
  where
    onPlain expr rest = collectIter (expr : results) rest

    onIf ifRate check th cons lhs exprs = do
      vs <- blockLocalVars ifRate th
      (newIfBlock, rest) <- redefineIf vs lhs ifRate check cons exprs
      toResult newIfBlock rest

    toResult newIfBlock rest = do
      collectIter (copyToResult newIfBlock results) rest

    copyToResult :: [a] -> [a] -> [a]
    copyToResult items result = List.foldl' (flip (:)) result items

    blockLocalVars ifRate root = do
      localUsage <- getLocalUsage ifRate root
      -- globals <- mapM (\v -> (\g -> (v, (g, localUsage IntMap.! v))) <$> readGlobalUsages v) $ IntMap.keys localUsage
      -- trace (unlines $ show <$> globals) $
      getLocalVars localUsage ifRate root

    onIfElse ifRate check th el cons lhs exprs = do
      thVars <- blockLocalVars ifRate th
      elVars <- blockLocalVars ifRate el
      (newIfBlock, rest) <- redefineIfElse thVars elVars lhs ifRate check cons exprs
      toResult newIfBlock rest

    onIfExp ifRate check th el lhs exprs = do
      thVars <- blockLocalVars ifRate th
      elVars <- blockLocalVars ifRate el
      (newIfBlock, rest) <- redefineIfElseExp thVars elVars th el lhs ifRate check cons exprs
      toResult newIfBlock rest
      where
        cons = IfElseCons{ifElseBegin = IfBegin, elseBegin = ElseBegin, ifElseEnd = IfEnd}

collectSubs :: Bool -> [Expr] -> Collect s [Expr]
collectSubs hasIfs newIfBlock
  | hasIfs = List.reverse <$> collectIter [] newIfBlock
  | otherwise = pure newIfBlock

redefineIf ::
  LocalVars ->
  Var ->
  IfRate ->
  CondInfo (PrimOr Var) ->
  IfCons (PrimOr Var) ->
  [Expr] ->
  Collect s ([Expr], [Expr])
redefineIf localVars ifBeginId ifRate condInfo IfCons{..} exprs = do
  ifStmts <- getIfStmts
  first (toResult ifStmts) <$> iterRedefine ifRate localVars blockSize [] False [] exprs
  where
    blockSize = IntSet.size localVars

    -- \| we expect if-block expressions to be reversed
    toResult (ifBeginStmt, ifEndStmt) blockExprs =
      ifEndStmt : blockExprs <> [ifBeginStmt]

    getIfStmts = do
      ifEndId <- freshId
      let
        ifEndStmt = Stmt (Var Xr ifEndId) (toRatedExp ifEnd)
        ifBeginStmt = Stmt ifBeginId (toRatedExp $ ifBegin ifRate condInfo)
      pure (ifBeginStmt, ifEndStmt)

iterRedefine :: IfRate -> LocalVars -> Int -> [Expr] -> Bool -> [Expr] -> [Expr] -> Collect s ([Expr], [Expr])
iterRedefine ifRate localVars currentBlockSize resultIfExprs hasIfs resultRest nextExprs
  | currentBlockSize <= 0 = result
  | otherwise =
      case nextExprs of
        [] -> result
        e@(Stmt lhs _) : es ->
          if isLocal lhs
            then appendLocal e es
            else appendRest e es
  where
    rec onBlockSize expr onIfExprs onRestExprs newNextExprs =
      iterRedefine
        ifRate
        localVars
        (onBlockSize currentBlockSize)
        (onIfExprs resultIfExprs)
        (hasIfs || (isIfExpr $ stmtRhs expr))
        (onRestExprs resultRest)
        newNextExprs

    result =
      recollect
        ( List.reverse $ resultIfExprs
        , hasIfs
        , List.reverse resultRest <> nextExprs
        )

    recollect (newIfBlock, finalHasIfs, rest) = do
      newIfBlockCollected <- collectSubs finalHasIfs newIfBlock
      pure (newIfBlockCollected, rest)

    appendLocal e es = rec pred e (e :) id es
    appendRest e es = rec id e id (e :) es

    isLocal :: Var -> Bool
    isLocal var = IntSet.member (varId var) localVars

redefineIfElse ::
  LocalVars ->
  LocalVars ->
  Var ->
  IfRate ->
  CondInfo (PrimOr Var) ->
  IfElseCons (PrimOr Var) ->
  [Expr] ->
  Collect s ([Expr], [Expr])
redefineIfElse thLocalVars elLocalVars ifBeginId ifRate condInfo IfElseCons{..} exprs = do
  ifStmts <- getIfElseStmts
  (ifBlockExprs, rest1) <- getIfPart exprs
  (elseBlockExprs, rest2) <- getElsePart rest1
  pure (toResult ifStmts ifBlockExprs elseBlockExprs, rest2)
  where
    -- note that block epxressions are reversed
    toResult (ifBeginStmt, elseBeginStmt, ifEndStmt) ifBlockExprs elseBlockExprs =
      ifEndStmt
        : mconcat
          [ elseBlockExprs
          , [elseBeginStmt]
          , ifBlockExprs
          , [ifBeginStmt]
          ]

    getIfElseStmts = do
      let
        ifBeginStmt = Stmt ifBeginId (toRatedExp $ ifElseBegin ifRate condInfo)
      elseBeginStmt <- (\elId -> Stmt (Var Xr elId) (toRatedExp elseBegin)) <$> freshId
      ifEndStmt <- (\endId -> Stmt (Var Xr endId) (toRatedExp ifElseEnd)) <$> freshId
      pure (ifBeginStmt, elseBeginStmt, ifEndStmt)

    getIfPart es = iterRedefine ifRate thLocalVars ifBlockSize [] False [] es
    getElsePart es = iterRedefine ifRate elLocalVars elseBlockSize [] False [] es

    ifBlockSize = IntSet.size thLocalVars
    elseBlockSize = IntSet.size elLocalVars

redefineIfElseExp ::
  forall s.
  LocalVars ->
  LocalVars ->
  PrimOr Var ->
  PrimOr Var ->
  Var ->
  IfRate ->
  CondInfo (PrimOr Var) ->
  IfElseCons (PrimOr Var) ->
  [Expr] ->
  Collect s ([Expr], [Expr])
redefineIfElseExp thLocalVars elLocalVars th el ifResultId ifRate condInfo IfElseCons{..} exprs = do
  ifStmts <- getIfElseStmts
  -- note that blocks are returned in reversed order
  (ifBlockExprs, rest1) <- getIfPart exprs
  (elseBlockExprs, rest2) <- getElsePart rest1
  ifResult <- toResult ifStmts ifBlockExprs elseBlockExprs
  pure (ifResult, rest2)
  where
    -- note that expressions in the blocks are returned in reversed order
    toResult :: (Expr, Expr, Expr) -> [Expr] -> [Expr] -> Collect s [Expr]
    toResult (ifBeginStmt, elseBeginStmt, ifEndStmt) ifBlockExprs elseBlockExprs = do
      thAssign <- writeRes ifResultId th
      elAssign <- writeRes ifResultId el
      pure $
        ifEndStmt
          : elAssign
          : mconcat
            [ elseBlockExprs
            , [elseBeginStmt, thAssign]
            , ifBlockExprs
            , [ifBeginStmt]
            ]

    getIfElseStmts = do
      ifBeginStmt <- (\ifBeginId -> (Stmt (Var Xr ifBeginId) $ toRatedExp $ ifElseBegin ifRate condInfo)) <$> freshId
      elseBeginStmt <- (\elId -> (Stmt (Var Xr elId) $ toRatedExp elseBegin)) <$> freshId
      ifEndStmt <- (\endId -> (Stmt (Var Xr endId) $ toRatedExp ifElseEnd)) <$> freshId
      pure (ifBeginStmt, elseBeginStmt, ifEndStmt)

    getIfPart es = iterRedefine ifRate thLocalVars ifBlockSize [] False [] es
    getElsePart es = iterRedefine ifRate elLocalVars elseBlockSize [] False [] es

    ifBlockSize = IntSet.size thLocalVars
    elseBlockSize = IntSet.size elLocalVars

    writeRes :: Var -> PrimOr Var -> Collect s Expr
    writeRes resId expr = do
      varWriteId <- freshId
      pure $
        Stmt
          { stmtLhs = Var Xr varWriteId
          , stmtRhs = toRatedExp $ WriteVar ifRate (toVar resId) expr
          }

    toVar v = Exp.VarVerbatim (varType v) name
      where
        name = Text.toLower $ Text.pack $ show (varType v) ++ show (varId v)

toRatedExp :: MainExp (PrimOr a) -> RatedExp a
toRatedExp expr =
  RatedExp
    { ratedExpHash = ExpHash ""
    , ratedExpDepends = Nothing
    , ratedExpRate = Nothing
    , ratedExpExp = expr
    }

type LocalMarks = IntMap Bool

getLocalVars :: forall s. LocalUsageCounts -> IfRate -> PrimOr Var -> Collect s LocalVars
getLocalVars localUsages ifRate root =
  toSet
    <$> traverseAccumDag update initMarks (isEnd ifRate) root
  where
    initMarks = either (const IntMap.empty) (\var -> IntMap.singleton (varId var) True) $ unPrimOr root

    update :: Expr -> LocalMarks -> Collect s LocalMarks
    update (Stmt lhs rhs) localMarks
      | isParentLocal = do
          isLocal <- fullyInsideLocal lhs
          let
            tfm = if isLocal then id else onFalseLocal
          -- when (varId lhs == 92)
          --   $ trace (unwords ["IS 92:", show isLocal]) $ pure ()
          pure $ tfm $ IntMap.alter (Just . maybe isLocal (isLocal &&)) (varId lhs) localMarks
      | otherwise = pure $ onFalseLocal localMarks
      where
        isParentLocal = fromMaybe True $ IntMap.lookup (varId lhs) localMarks

        onFalseLocal =
          execState (mapM_ (\v -> modify' $ IntMap.insert (varId v) False) rhs)

    fullyInsideLocal :: Var -> Collect s Bool
    fullyInsideLocal lhs = do
      globalCount <- readGlobalUsages (varId lhs)
      let
        localCount = IntMap.lookup (varId lhs) localUsages
      -- how to do node specific debug:
      -- when (varId lhs == 92)
      --  $ trace (unwords ["IS 92:", "global:", show globalCount, "local:", show localCount]) $ pure ()
      pure $ Just globalCount == localCount

    toSet :: LocalMarks -> LocalVars
    toSet = IntMap.keysSet . IntMap.filter id

getLocalUsage :: forall s. IfRate -> PrimOr Var -> Collect s LocalUsageCounts
getLocalUsage ifRate root =
  traverseAccumDag update initCount (isEnd ifRate) root
  where
    initCount = either (const IntMap.empty) (\var -> IntMap.singleton (varId var) 1) $ unPrimOr root

    update :: Expr -> LocalUsageCounts -> Collect s LocalUsageCounts
    update (Stmt _lhs rhs) st =
      pure $
        execState (mapM_ count rhs) st

    count var = modify' $ IntMap.alter (Just . maybe 1 succ) (varId var)

---------------------------------------------------------------------------

{-| Defines rule that if we are inside Kr if-block we can not bring inside
Ir-expressions
-}
isEnd :: IfRate -> Expr -> Collect s Bool
isEnd ifRate (Stmt lhs rhs)
  | isInitVar rhs = pure True
  | otherwise = case ifRate of
      IfIr -> pure False
      IfKr -> readIsInit (varId lhs)

isInitVar :: RatedExp Var -> Bool
isInitVar expr =
  case ratedExpExp expr of
    InitVar _ _ -> True
    InitArr _ _ -> True
    _ -> False

isIfExpr :: RatedExp Var -> Bool
isIfExpr rhs = case getExprType rhs of
  PlainType -> False
  _ -> True

getExprType :: RatedExp Var -> ExprType (PrimOr Var)
getExprType expr =
  case ratedExpExp expr of
    If rate c th el -> IfExpType rate c th el
    IfBlock rate c (CodeBlock th) -> IfType rate c th $ IfCons{ifBegin = IfBegin, ifEnd = IfEnd}
    IfElseBlock rate c (CodeBlock th) (CodeBlock el) ->
      -- trace (unlines ["TH/EL", show (th, el)])
      IfElseType rate c th el $ IfElseCons{ifElseBegin = IfBegin, elseBegin = ElseBegin, ifElseEnd = IfEnd}
    WhileBlock rate c (CodeBlock th) -> IfType rate c th $ IfCons{ifBegin = WhileBegin, ifEnd = WhileEnd}
    UntilBlock rate c (CodeBlock th) -> IfType rate c th $ IfCons{ifBegin = UntilBegin, ifEnd = UntilEnd}
    _ -> PlainType
