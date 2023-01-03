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
  ) where

import Control.Monad
import qualified Csound.Dynamic.Tfm.DeduceTypes as D
import Data.List qualified as List
import Csound.Dynamic.Types hiding (Exp, Var)
import Csound.Dynamic.Types.Exp qualified as E
import Data.Semigroup (Max (..))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Control.Monad.Trans.State.Strict
import Data.Text qualified as Text

type Var  = D.Var Rate

type Lhs = [Var]
type Rhs = RatedExp Var
type Exp = (Lhs, Rhs)


saturateIf :: [Exp] -> [Exp]
saturateIf dag = List.reverse $ go IntSet.empty $ List.reverse dag
  where
    go :: IntSet -> [Exp] -> [Exp]
    go freeVars = \case
      [] -> []
      (exprIds, expr) : exprs -> case ratedExpExp expr of
        If cond th el ->
          let
            localFreeVars = fromCond cond
            (thExprs, elExprs, restExprs) = saturateIfExpr (freeVars <> localFreeVars) exprIds th el exprs
            thExprsSaturated = go localFreeVars thExprs
            elExprsSaturated = go localFreeVars elExprs
            restExprsSaturated = go freeVars restExprs
            ifBeginExpr = expr { ratedExpExp = IfBegin (getCondRate cond) cond }
            elseExpr = expr { ratedExpRate = Nothing, ratedExpExp = ElseBegin }
            endExpr = expr { ratedExpRate = Nothing, ratedExpExp = IfEnd }
          in mconcat
            [ [([], endExpr)]
            , elExprsSaturated
            , [([], elseExpr)]
            , thExprsSaturated
            , [([], ifBeginExpr)]
            , restExprsSaturated
            ]
        _ ->
          let
            newFreeVars = (freeVars `IntSet.union` fromRatedExp expr) `IntSet.difference` fromVars exprIds
            exprsSaturated = go newFreeVars exprs
          in
            (exprIds, expr) : exprsSaturated

getCondRate :: CondInfo (PrimOr Var) -> Rate
getCondRate = max Kr . getMax . foldMap (Max . getRate)
  where
    getRate = either (const Kr) ratedVarRate . unPrimOr


saturateIfExpr :: IntSet -> [Var] -> PrimOr Var -> PrimOr Var -> [Exp] -> ([Exp], [Exp], [Exp])
saturateIfExpr freeVars resIds ifExp elseExp exps =
  toResult $ stRes $ execState (go exps) $ initSt freeVars ifExp elseExp
  where
    toResult Res{..} =
      ( List.reverse $ ifExps ++ map (flip writeRes ifExp) resIds
      , List.reverse $ elseExps ++ map (flip writeRes elseExp) resIds
      , List.reverse restExps
      )

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

    toVar v = E.VarVerbatim (D.varType v) name
      where
        name = Text.toLower $ Text.pack $ show (D.varType v) ++ show (D.varId v)

    go :: [Exp] -> State St ()
    go = \case
      [] -> pure ()
      (ids, expr) : rest -> do
        filt <- gets stFilter
        let isIfNotOk = ifNotActive filt
            isElseNotOk = elseNotActive filt
        if (isIfNotOk && isElseNotOk)
          then fin ((ids, expr) : rest)
          else do
            process isIfNotOk isElseNotOk (ids, expr)
            go rest

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
    toIf = toBy updateIf updateIfFilter
      where
        updateIf item res = res { ifExps = item : ifExps res }

        updateIfFilter excludeIds includeIds filt =
          filt
            { ifIds = (ifIds filt `IntSet.difference` excludeIds) `IntSet.union` includeIds
            }

    toElse :: Exp -> State St ()
    toElse = toBy updateElse updateElseFilter
      where
        updateElse item res = res { elseExps = item : elseExps res }

        updateElseFilter excludeIds includeIds filt =
          filt
            { elseIds = (elseIds filt `IntSet.difference` excludeIds) `IntSet.union` includeIds
            }

    toBy :: (Exp -> Res -> Res) -> (IntSet -> IntSet -> IfThenFilter -> IfThenFilter) -> Exp -> State St ()
    toBy updateRes updateFilter e@(ids, expr) = modify' $ \st ->
      st
        { stRes = updateRes e (stRes st)
        , stFilter = updateFilter oldIds newIds (stFilter st)
        }
      where
        oldIds = IntSet.fromList $ map D.varId ids

        newIds = foldMap (either (const IntSet.empty) (IntSet.singleton . D.varId) . unPrimOr) $ ratedExpExp expr

    toRest :: Exp -> State St ()
    toRest = toBy updateRest updateRestFilter
      where
        updateRest item res = res { restExps = item : restExps res }

        updateRestFilter oldIds newIds filt =
          filt
            { restIds = mconcat [restIds filt, oldIds, newIds]
            }

initSt :: IntSet -> PrimOr Var -> PrimOr Var -> St
initSt freeVars ifExp elseExp =
  St
    { stRes = Res [] [] []
    , stFilter =
        IfThenFilter
          { ifIds   = fromExp ifExp
          , elseIds = fromExp elseExp
          , restIds = freeVars
          }
    }

fromVars :: [Var] -> IntSet
fromVars = IntSet.fromList . map D.varId

fromExp :: PrimOr Var -> IntSet
fromExp (PrimOr eVar) = either (const IntSet.empty) (IntSet.singleton . D.varId) eVar

fromCond :: CondInfo (PrimOr Var) -> IntSet
fromCond = foldMap fromExp

fromRatedExp :: RatedExp Var -> IntSet
fromRatedExp = foldMap fromExp  . ratedExpExp

data St = St
  { stRes :: Res
  , stFilter :: IfThenFilter
  }

data Res = Res
  { ifExps :: ![Exp]
  , elseExps :: ![Exp]
  , restExps :: ![Exp]
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
     all (flip IntSet.member includeSet . D.varId) ids
  && not (any (flip IntSet.member excludeSet . D.varId) ids)
