{-# LANGUAGE TupleSections #-}

module Csound.Dynamic.Tfm.UnfoldMultiOuts (
  unfoldMultiOuts,
  Selector (..),
) where

import Control.Monad (join)
import Control.Monad.Trans.State.Strict
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IM
import Data.Ord (comparing)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Csound.Dynamic.Build (getRates, isMultiOutSignature)
import Csound.Dynamic.Tfm.InferTypes (InferenceResult (..), Stmt (..), Var (..))
import Csound.Dynamic.Types.Exp hiding (Var (..))

type ChildrenMap = IM.IntMap (Seq Port)

lookupChildren :: ChildrenMap -> Var -> Seq Port
lookupChildren m parentVar =
  case IM.lookup (varId parentVar) m of
    Just ports -> ports
    Nothing -> error $ "Invalid children map for id: " <> (show $ varId parentVar)

mkChildrenMap :: [(Var, Selector)] -> ChildrenMap
mkChildrenMap = IM.fromListWith (<>) . fmap extract
  where
    extract (var, sel) =
      ( varId $ selectorParent sel
      , return $ Port (varId var) (selectorOrder sel)
      )

data Port = Port
  { portId :: Int
  , portOrder :: Int
  }
  deriving (Show)

type SingleStmt = Stmt Var
type MultiStmt = ([Var], RatedExp Var)

data Selector = Selector
  { selectorParent :: Var
  , selectorOrder :: Int
  }

unfoldMultiOuts :: InferenceResult -> ([MultiStmt], Int)
unfoldMultiOuts InferenceResult{..} = runState (fmap (fmap $ first toList) st) programLastFreshId
  where
    (noSelectorStmts, selectors) =
      partitionEithers $
        fmap (\stmt@(Stmt lhs rhs) -> maybe (Left stmt) (Right . (lhs,)) $ getSelector rhs) typedProgram

    st = mapM (unfoldStmt $ mkChildrenMap selectors) $ noSelectorStmts

unfoldStmt :: ChildrenMap -> SingleStmt -> State Int (Seq Var, RatedExp Var)
unfoldStmt childrenMap (Stmt lhs rhs) =
  case getParentTypes rhs of
    Nothing -> return (Seq.singleton lhs, rhs)
    Just types -> fmap (,rhs) $ formLhs (lookupChildren childrenMap lhs) types

formLhs :: Seq Port -> Seq Rate -> State Int (Seq Var)
formLhs ports types = fmap (Seq.zipWith Var types) (getPorts ports)
  where
    getPorts ps = state $ \lastFreshId ->
      let
        ps' = Seq.sortBy (comparing portOrder) ps
        (ids, lastPortOrder) = runState (mapM (fillMissingPorts lastFreshId) ps') 0
        freshIdForTail = 1 + lastFreshId + inUsePortsSize
        tailIds = fmap (+ freshIdForTail) $ Seq.fromList [0 .. outputArity - 1 - lastPortOrder]
       in
        (join ids <> tailIds, lastFreshId + outputArity - inUsePortsSize)

    outputArity = length types
    inUsePortsSize = length ports

    fillMissingPorts :: Int -> Port -> State Int (Seq Int)
    fillMissingPorts lastFreshId port = state $ \s ->
      if s == order
        then (Seq.singleton e, next)
        else (fmap (+ lastFreshId) (Seq.fromList [s .. order - 1]) Seq.|> e, next)
      where
        e = portId port
        order = portOrder port
        next = order + 1

-----------------------------------------------------------------------
-- unfolds multiple rates generic functions

getSelector :: RatedExp Var -> Maybe Selector
getSelector x =
  case ratedExpExp x of
    Select _ order (PrimOr (Right parent)) -> Just $ Selector parent order
    _ -> Nothing

getParentTypes :: RatedExp Var -> Maybe (Seq Rate)
getParentTypes x =
  case ratedExpExp x of
    Tfm i _ -> fromInfo i
    ExpPrim (PrimTmpVar v) -> fromInfo =<< tmpVarInfo v
    _ -> Nothing
  where
    fromInfo i
      | isMultiOutSignature $ infoSignature i = Just (Seq.fromList $ getRates $ ratedExpExp x)
      | otherwise = Nothing
