-- | In this module we substitute temporary vars with graph vars
module Csound.Dynamic.Tfm.TmpVars
  ( removeTmpVars
  ) where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe

import Csound.Dynamic.Types.Exp (RatedExp (..), TmpVar (..), MainExp (..), PrimOr (..), Prim (..))

type Dag f = [(Int, f Int)]

type RemoveTmp a = State (IntMap Int) a

removeTmpVars :: Dag RatedExp -> Dag RatedExp
removeTmpVars dag = evalState (mapM (substArgs <=< saveTmpVar) dag) IntMap.empty
  where
    saveTmpVar :: (Int, RatedExp Int) -> RemoveTmp (Int, RatedExp Int)
    saveTmpVar (resId, expr) = case ratedExpExp expr of
      ReadVarTmp tmp v -> do
        insertTmpVar tmp resId
        pure (resId, expr { ratedExpExp = ReadVar v })
      TfmInit tmp info args -> do
        insertTmpVar tmp resId
        pure (resId, expr { ratedExpExp = Tfm info args })
      _ -> pure (resId, expr)

    substArgs :: (Int, RatedExp Int) -> RemoveTmp (Int, RatedExp Int)
    substArgs (resId, expr) = do
      e <- mapM (substTmp resId) (ratedExpExp expr)
      pure $ (resId, expr { ratedExpExp = e })

    substTmp :: Int -> PrimOr Int -> RemoveTmp (PrimOr Int)
    substTmp resId (PrimOr e) = fmap PrimOr $ case e of
      Right n -> pure (Right n)
      Left p -> case p of
        PrimTmpVar tmp -> Right <$> lookupTmpVar resId tmp
        _              -> pure $ Left p

insertTmpVar :: TmpVar -> Int -> RemoveTmp ()
insertTmpVar (TmpVar v) resId = modify' $ IntMap.insert v resId

lookupTmpVar :: Int -> TmpVar -> RemoveTmp Int
lookupTmpVar resId (TmpVar n) = gets (fromMaybe err . IntMap.lookup n)
  where
    err = error $ "TmpVar not found: " <> show n <> " on res: " <> show resId

