-- | In this module we substitute temporary vars with graph vars
--
-- After this stage no TmpVars should left in the DAg.
-- typical problems: TmpVar is not inlined by some reason in the next expression down the flow.
--
-- Assumption: every tmpVar is inlined in the next expression
module Csound.Dynamic.Tfm.TmpVars
  ( removeTmpVars
  ) where

import Control.Monad.Trans.State.Strict
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe
import Control.Monad
-- import Debug.Trace

import Csound.Dynamic.Types.Exp
  ( RatedExp (..),
    TmpVar (..),
    MainExp (..),
    PrimOr (..),
    Prim (..),
    Rate (..),
    getTmpVars,
    IfRate (..),
    Info (..),
    TmpVarRate (..),
    getSingleTmpRate,
  )

type Node f = (Int, f Int)
type Dag f = [Node f]

type RemoveTmp a = State St a

data St = St
  { stIds :: IntMap Int
    -- ^ ids of tmp vars LHS in equations
  , stRates :: IntMap (Maybe TmpVarRate, Maybe Info)
    -- ^ rates if requested for TmpVar's
  }
  deriving (Show)

removeTmpVars :: Dag RatedExp -> Dag RatedExp
removeTmpVars dag = flip evalState (St IntMap.empty IntMap.empty) $ do
  mapM_ (mapM_ saveTmpVarRate . getTmpVars . ratedExpExp .  snd) dag
  mapM (substArgs <=< saveTmpVar) dag

  where
    requestRate ifRate mRate =
      case ifRate of
        IfIr -> Just Ir
        _ -> mRate

    saveTmpVar :: (Int, RatedExp Int) -> RemoveTmp (Int, RatedExp Int)
    saveTmpVar (resId, expr) = case ratedExpExp expr of
      ReadVarTmp ifRate tmp v -> do
        mRate <- lookupRate tmp
        insertTmpVar tmp resId
        pure $
          -- (\x -> trace (unwords ["TMP VAR:", show $ratedExpRate $ snd x]) x) $
          (resId, expr { ratedExpExp = ReadVar ifRate v, ratedExpRate = requestRate ifRate mRate })

      ReadArrTmp ifRate tmp v index -> do
        mRate <- lookupRate tmp
        insertTmpVar tmp resId
        pure $ (resId, expr { ratedExpExp = ReadArr ifRate v index, ratedExpRate = requestRate ifRate mRate })

      TfmInit tmp info args -> do
        mTmpRate <- lookupTmpRate tmp
        let
          onSingleRate mRate = do
            insertTmpVar tmp resId
            pure (resId, expr { ratedExpExp = Tfm info args, ratedExpRate = mRate })

          onMultiRate = do
            insertTmpVar tmp resId
            pure (resId, expr { ratedExpExp = Tfm info args, ratedExpRate = Nothing })
        case mTmpRate of
          Just tmpRate ->
            case tmpRate of
              SingleTmpRate rate -> onSingleRate (Just rate)
              MultiTmpRate _rates -> onMultiRate
          Nothing -> onSingleRate Nothing

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
insertTmpVar (TmpVar _ _ v) resId =
  modify' $ \st -> st { stIds = IntMap.insert v resId (stIds st) }

lookupTmpVar :: Int -> TmpVar -> RemoveTmp Int
lookupTmpVar resId (TmpVar _ _ n) = gets (fromMaybe err . IntMap.lookup n . stIds)
  where
    err = error $ "TmpVar not found: " <> show n <> " on result id: " <> show resId

saveTmpVarRate :: TmpVar -> RemoveTmp ()
saveTmpVarRate (TmpVar mRate mInfo n) = do
  modify' $ \st -> st { stRates = IntMap.insert n (mRate, mInfo) (stRates st)}

lookupRate :: TmpVar -> RemoveTmp (Maybe Rate)
lookupRate var =
  fmap (getSingleTmpRate =<<) (lookupTmpRate var)

lookupTmpRate :: TmpVar -> RemoveTmp (Maybe TmpVarRate)
lookupTmpRate (TmpVar _ _ n) = gets (fst <=< (IntMap.lookup n . stRates))
