-- | In this module we substitute temporary vars with graph vars
module Csound.Dynamic.Tfm.TmpVars
  ( removeTmpVars
  ) where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe

import Csound.Dynamic.Types.Exp (RatedExp (..), TmpVar (..), MainExp (..), PrimOr (..), Prim (..), Rate (..))

type Node f = (Int, f Int)
type Dag f = [Node f]

type RemoveTmp a = State St a

data St = St
  { stIds :: IntMap Int
    -- ^ ids of tmp vars LHS in equations
  , stRates :: IntMap Rate
    -- ^ rates if requested for TmpVar's
  }

removeTmpVars :: Dag RatedExp -> Dag RatedExp
removeTmpVars dag = flip evalState (St IntMap.empty IntMap.empty) $ do
  mapM_ saveRates dag
  mapM (substArgs <=< saveTmpVar) dag
  where
    saveTmpVar :: (Int, RatedExp Int) -> RemoveTmp (Int, RatedExp Int)
    saveTmpVar (resId, expr) = case ratedExpExp expr of
      ReadVarTmp tmp v -> do
        mRate <- lookupRate tmp
        insertTmpVar tmp resId
        pure (resId, expr { ratedExpExp = ReadVar v, ratedExpRate = mRate })
      TfmInit tmp info args -> do
        mRate <- lookupRate tmp
        insertTmpVar tmp resId
        pure (resId, expr { ratedExpExp = Tfm info args, ratedExpRate = mRate })
      _ -> pure (resId, expr)

    substArgs :: (Int, RatedExp Int) -> RemoveTmp (Int, RatedExp Int)
    substArgs (resId, expr) = do
      e <- mapM (substTmp resId) (ratedExpExp expr)
      pure $ (resId, expr { ratedExpExp = e })

    saveRates :: Node RatedExp -> RemoveTmp ()
    saveRates (_resId, expr) = case ratedExpExp expr of
      ExpPrim (PrimTmpVar n) -> mapM_ (saveTmpVarRate n) (ratedExpRate expr)
      _ -> pure ()

    substTmp :: Int -> PrimOr Int -> RemoveTmp (PrimOr Int)
    substTmp resId (PrimOr e) = fmap PrimOr $ case e of
      Right n -> pure (Right n)
      Left p -> case p of
        PrimTmpVar tmp -> Right <$> lookupTmpVar resId tmp
        _              -> pure $ Left p

insertTmpVar :: TmpVar -> Int -> RemoveTmp ()
insertTmpVar (TmpVar v) resId = modify' $ \st -> st { stIds = IntMap.insert v resId (stIds st) }

lookupTmpVar :: Int -> TmpVar -> RemoveTmp Int
lookupTmpVar resId (TmpVar n) = gets (fromMaybe err . IntMap.lookup n . stIds)
  where
    err = error $ "TmpVar not found: " <> show n <> " on res: " <> show resId

saveTmpVarRate :: TmpVar -> Rate -> RemoveTmp ()
saveTmpVarRate (TmpVar n) rate =
  modify' $ \st -> st { stRates = IntMap.insert n rate $ stRates st }

lookupRate :: TmpVar -> RemoveTmp (Maybe Rate)
lookupRate (TmpVar n) = gets (IntMap.lookup n . stRates)
