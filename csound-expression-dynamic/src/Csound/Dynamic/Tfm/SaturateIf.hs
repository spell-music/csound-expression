-- | Saturates if-expressions.
-- It turns expressions like:
--
-- > a1 = expr1
-- > a2 = expr2
-- > if cond then
-- >  a3 = a1
-- > esle
-- >  a3 = a2
-- > endif
--
-- to expressions with all subpexpressions brought inside if.
-- it leads to more efficient code:
--
-- > if cond then
-- >   a1 = expr1
-- >   a3 = a1
-- > else
-- >   a2 = expr2
-- >   a3 = a2
-- > endif
module Csound.Dynamic.Tfm.SaturateIf
  ( saturateIf
  , SaturateIfOptions (..)
  ) where

import Control.Monad
import Csound.Dynamic.Tfm.InferTypes (Var (..))
import Data.List qualified as List
import Csound.Dynamic.Types hiding (Exp, Var (..))
import Csound.Dynamic.Types.Exp qualified as E
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Control.Monad.Trans.State.Strict
import Data.Default
import Data.Text qualified as Text
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Maybe

type Lhs = [Var]
type Rhs = RatedExp Var
type Exp = (Lhs, Rhs)

data SaturateIfOptions = SaturateIfOptions
  { saturateIfStatements :: Bool -- ^ flag to turn off if-saturation for statements
  }

instance Default SaturateIfOptions where
  def =
    SaturateIfOptions
      { saturateIfStatements = True
      }

saturateIf :: SaturateIfOptions -> [Exp] -> [Exp]
saturateIf SaturateIfOptions{..} dag =
  List.reverse $ DList.toList $ go IntSet.empty $ List.reverse dag
  where
    go :: IntSet -> [Exp] -> DList Exp
    go freeVars = \case
      [] -> DList.empty
      (exprIds, expr) : exprs -> case ratedExpExp expr of
        If ifRate cond th el ->
          let
            localFreeVars = fromCond cond
            newFreeVars = localFreeVars <> freeVars
          in  processRes ifRate localFreeVars cond expr newFreeVars $ saturateIfExpr ifRate newFreeVars exprIds th el exprs
        IfEnd | saturateIfStatements ->
          let
            (Res ths els restExprs, (ifRate, cond)) = getIfElseBlock exprs
            localFreeVars = fromCond cond
            newFreeVars = localFreeVars <> freeVars
          in processRes ifRate localFreeVars cond expr newFreeVars $ saturateIfStatement ifRate newFreeVars ths els restExprs
        _ ->
          let
            newFreeVars = (freeVars `IntSet.union` fromRatedExp expr) `IntSet.difference` fromVars exprIds
            exprsSaturated = go newFreeVars exprs
          in
            DList.singleton (exprIds, expr) <> exprsSaturated

    processRes :: E.IfRate -> IntSet -> CondInfo (PrimOr Var) -> RatedExp Var -> IntSet -> Res -> DList Exp
    processRes ifRate localFreeVars cond expr freeVars (Res thExprs elExprs restExprs) =
      mconcat
        [ DList.singleton ([], endExpr)
        , elExprsSaturated
        , DList.singleton ([], elseExpr)
        , thExprsSaturated
        , DList.singleton ([], ifBeginExpr)
        , restExprsSaturated
        ]
      where
        thExprsSaturated = go localFreeVars thExprs
        elExprsSaturated = go localFreeVars elExprs
        restExprsSaturated = go freeVars restExprs
        ifBeginExpr = expr { ratedExpExp = IfBegin ifRate cond }
        elseExpr = expr { ratedExpRate = Nothing, ratedExpExp = ElseBegin }
        endExpr = expr { ratedExpRate = Nothing, ratedExpExp = IfEnd }

data IfBlockSt = IfBlockSt
  { nestingCount :: !Int
  , ifRes        :: !Res
  , currentBlock :: !CurrentBlock
  , ifArgs       :: !(Maybe (IfRate, CondInfo (PrimOr Var)))
  }


data CurrentBlock = IffBlock | ElseBlock | EndBlock

-- | we go over expressions in reverse and accumulate
-- all blocks for if-then-else-endif statemnt.
--
-- We first encounter endif then we go into else or if-begin
-- and after that we go over rest.
--
-- We use nesting counter to not to be distracted by nested if-statements
getIfElseBlock :: [Exp] -> (Res, (IfRate, CondInfo (PrimOr Var)))
getIfElseBlock es =
  (reverseRes $ ifRes blocks, fromMaybe (defRate, defCond) $ ifArgs blocks)
  where
    blocks = execState (mapM_ go es) initSt

    defRate = E.IfKr
    defCond = Inline (InlineExp TrueOp []) mempty

    initSt :: IfBlockSt
    initSt =
      IfBlockSt
        { ifRes = Res [] [] []
        , nestingCount = 0
        , currentBlock = EndBlock
        , ifArgs = Nothing
        }

    go, onIf, onElse, onEnd, onProcess, save :: Exp -> State IfBlockSt ()

    go expr = do
      block <- gets currentBlock
      case block of
        IffBlock -> save expr
        _       -> onProcess expr

    onProcess expr
      | isIfBegin expr   = onIf expr
      | isElseBegin expr = onElse expr
      | isIfEnd expr     = onEnd expr
      | otherwise        = save expr

    onIf expr = do
      count <- gets nestingCount
      if (count > 0)
        then do
          updateNesting pred
          save expr
        else do
          block <- gets currentBlock
          case block of
            -- there were no else-block
            EndBlock -> moveElseToIf
            _        -> pure ()

          setBlock IffBlock
          modify' $ \st -> st { ifArgs = getIfArgs expr }

    onElse expr = do
      count <- gets nestingCount
      if (count > 0)
        then save expr
        else setBlock ElseBlock

    onEnd expr = do
      updateNesting succ
      save expr

    save expr = do
      block <- gets currentBlock
      case block of
        IffBlock   -> modify' $ \st -> st { ifRes = appendRest expr $ ifRes st }
        ElseBlock -> modify' $ \st -> st { ifRes = appendIf expr $ ifRes st }
        EndBlock  -> modify' $ \st -> st { ifRes = appendElse expr $ ifRes st }

    setBlock :: CurrentBlock -> State IfBlockSt ()
    setBlock block = modify' $ \s -> s { currentBlock = block }

    updateNesting :: (Int -> Int) -> State IfBlockSt ()
    updateNesting f = modify' $ \s -> s { nestingCount = f (nestingCount s) }

    moveElseToIf :: State IfBlockSt ()
    moveElseToIf = modify' $ \s -> s { ifRes = move $ ifRes s }
      where
        move Res{..} = Res { ifExps = elseExps, elseExps = [], restExps = restExps }

    isIfBegin, isElseBegin, isIfEnd :: Exp -> Bool

    isIfBegin = overRatedExpr $ \case
      IfBegin _ _ -> True
      _           -> False

    isElseBegin = overRatedExpr $ \case
      ElseBegin -> True
      _         -> False

    isIfEnd = overRatedExpr $ \case
      IfEnd -> True
      _     -> False

    getIfArgs (_, expr) =
      case ratedExpExp expr of
        IfBegin rate cond -> Just (rate, cond)
        _                 -> Nothing

    overRatedExpr p (_, expr) = p (ratedExpExp expr)

saturateIfExpr :: IfRate -> IntSet -> [Var] -> PrimOr Var -> PrimOr Var -> [Exp] -> Res
saturateIfExpr ifRate freeVars resIds ifExp elseExp exps =
  collectIfs ifRate initSt exps
  where
    initSt =
      St
        { stRes = Res (map (flip writeRes ifExp) resIds) (map (flip writeRes elseExp) resIds) []
        , stFilter =
            IfThenFilter
              { ifIds   = fromExp ifExp
              , elseIds = fromExp elseExp
              , restIds = freeVars
              }
        }

    writeRes :: Var -> PrimOr Var -> Exp
    writeRes resId expr =
      ( []
      , RatedExp
          { ratedExpHash = ""
          , ratedExpRate = Nothing
          , ratedExpDepends = Nothing
          , ratedExpExp = WriteVar (toVar resId) expr
          }
      )

    toVar v = E.VarVerbatim (varType v) name
      where
        name = Text.toLower $ Text.pack $ show (varType v) ++ show (varId v)

saturateIfStatement :: IfRate -> IntSet -> [Exp] -> [Exp] -> [Exp] -> Res
saturateIfStatement ifRate freeVars ifStmts elseStmts exps =
  collectIfs ifRate initSt exps
  where
    toSet stmts = foldMap (fromRatedExp . snd) stmts `IntSet.difference`  freeVars

    initSt =
      St
        { stRes = Res (List.reverse ifStmts) (List.reverse elseStmts) []
        , stFilter = IfThenFilter
            { ifIds   = toSet ifStmts
            , elseIds = toSet elseStmts
            , restIds = freeVars
            }
        }

collectIfs :: IfRate -> St -> [Exp] -> Res
collectIfs ifRate initSt exps = reverseRes $ stRes $ execState (go exps) initSt
  where
    go :: [Exp] -> State St ()
    go = \case
      [] -> pure ()
      (ids, expr) : rest -> do
        filt <- gets stFilter
        let isIfNotOk = ifNotActive filt
            isElseNotOk = elseNotActive filt
        if (isIfNotOk && isElseNotOk) || isIrInsideKr ids expr
          then fin ((ids, expr) : rest)
          else do
            process isIfNotOk isElseNotOk (ids, expr)
            go rest

    -- we can not put Ir-statements inside Kr if-then block
    isIrInsideKr ids expr = ifRate == IfKr && (hasIrOuts || hasKrConvertStmt)
      where
        -- we can not use Ir outputs inside if-block
        hasIrOuts   = any ((== Ir) . varType) ids

        -- we can not convert to Kr-inside if-block
        hasKrConvertStmt =
          case ratedExpExp expr of
            ConvertRate Kr (Just Ir) _ -> True
            _ -> False

    fin :: [Exp] -> State St ()
    fin rest = modify' $ \st -> st { stRes = updateRest $ stRes st }
      where
        updateRest res = res { restExps = List.foldl' (flip (:)) (restExps res) rest }

    process :: Bool -> Bool -> Exp -> State St ()
    process isIfNotOk isElseNotOk expr = do
      inIf <-
        if isIfNotOk
          then pure False
          else processIf expr
      inElse <-
        if isElseNotOk
          then pure False
          else processElse expr
      unless (inIf || inElse) $ toRest expr

    processIf :: Exp -> State St Bool
    processIf = processBy isIf toIf

    processElse :: Exp -> State St Bool
    processElse = processBy isElse toElse

    processBy :: (IfThenFilter -> Exp -> Bool) -> (Exp -> State St ()) -> Exp -> State St Bool
    processBy isCond toCond expr = do
      filt <- gets stFilter
      let inElse = isCond filt expr
      when inElse $ toCond expr
      pure inElse

    toIf :: Exp -> State St ()
    toIf = toBy appendIf updateIfFilter
      where
        updateIfFilter excludeIds includeIds filt =
          filt
            { ifIds = (ifIds filt `IntSet.difference` excludeIds) `IntSet.union` includeIds
            }

    toElse :: Exp -> State St ()
    toElse = toBy appendElse updateElseFilter
      where
        updateElseFilter excludeIds includeIds filt =
          filt
            { elseIds = (elseIds filt `IntSet.difference` excludeIds) `IntSet.union` includeIds
            }

    toRest :: Exp -> State St ()
    toRest = toBy appendRest updateRestFilter
      where
        updateRestFilter oldIds newIds filt =
          filt
            { restIds = mconcat [restIds filt, oldIds, newIds]
            }

    toBy :: (Exp -> Res -> Res) -> (IntSet -> IntSet -> IfThenFilter -> IfThenFilter) -> Exp -> State St ()
    toBy updateRes updateFilter e@(ids, expr) = modify' $ \st ->
      st
        { stRes = updateRes e (stRes st)
        , stFilter = updateFilter oldIds newIds (stFilter st)
        }
      where
        oldIds = IntSet.fromList $ map varId ids

        newIds = foldMap (either (const IntSet.empty) (IntSet.singleton . varId) . unPrimOr) $ ratedExpExp expr
fromVars :: [Var] -> IntSet
fromVars = IntSet.fromList . map varId

fromExp :: PrimOr Var -> IntSet
fromExp (PrimOr eVar) = either (const IntSet.empty) (IntSet.singleton . varId) eVar

fromCond :: CondInfo (PrimOr Var) -> IntSet
fromCond = foldMap fromExp

fromRatedExp :: RatedExp Var -> IntSet
fromRatedExp = foldMap fromExp  . ratedExpExp

data St = St
  { stRes    :: !Res
  , stFilter :: !IfThenFilter
  }

data Res = Res
  { ifExps   :: ![Exp]
  , elseExps :: ![Exp]
  , restExps :: ![Exp]
  }

appendIf :: Exp -> Res -> Res
appendIf item res = res { ifExps = item : ifExps res }

appendElse :: Exp -> Res -> Res
appendElse item res = res { elseExps = item : elseExps res }

appendRest :: Exp -> Res -> Res
appendRest item res = res { restExps = item : restExps res }

reverseRes :: Res -> Res
reverseRes Res{..} =
  Res
    { ifExps = List.reverse ifExps
    , elseExps = List.reverse elseExps
    , restExps = List.reverse restExps
    }

data IfThenFilter = IfThenFilter
  { ifIds   :: !IntSet
  , elseIds :: !IntSet
  , restIds :: !IntSet
  }

ifNotActive :: IfThenFilter -> Bool
ifNotActive IfThenFilter{..} = IntSet.null ifIds

elseNotActive :: IfThenFilter -> Bool
elseNotActive IfThenFilter{..} = IntSet.null elseIds

isIf :: IfThenFilter -> Exp -> Bool
isIf (IfThenFilter ifSet _ restSet) (ids, _) = isCondMember ifSet restSet ids

isElse :: IfThenFilter -> Exp -> Bool
isElse (IfThenFilter _ elseSet restSet) (ids, _) = isCondMember elseSet restSet ids

isCondMember :: IntSet -> IntSet -> [Var] -> Bool
isCondMember includeSet excludeSet ids =
     all (flip IntSet.member includeSet . varId) ids
  && not (any (flip IntSet.member excludeSet . varId) ids)

