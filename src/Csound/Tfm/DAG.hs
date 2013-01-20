module Csound.Tfm.DAG (
        cse, dag, Dag,
        letCse, Let(..),
        letCata, letCataM,
        module Csound.Tfm.BiMap,
        module Control.Monad.Trans.State.Strict,
        hashcons)
where

import Control.Applicative hiding (empty)

import Data.Fix
import Csound.Tfm.BiMap
import Control.Monad.Trans.State.Strict

import Data.Traversable
import Data.Hashable

import qualified Data.IntMap as IM

type Dag f = [(Int, f Int)]

dag :: (Eq (f Int), Ord (f Int), Traversable f) => Fix f -> Dag f
dag = toDag . cse

toDag :: BiMap (f Int) -> Dag f
toDag (BiMap _ a) = IM.toList a

cse :: (Eq (f Int), Ord (f Int), Traversable f) => Fix f -> BiMap (f Int)
cse x = execState (cataM hashcons x) empty


-- | Explicit sharing.
data Let f a    = LetExp (f a)
                | LetBind a (a -> a)


letCse :: (Eq (f Int), Ord (f Int), Traversable f) 
    => Fix (Let f) -> BiMap (f Int)
letCse x = execState (letCataM hashcons x) empty


letCataM :: (Applicative m, Monad m, Traversable f) => 
    (f a -> m a) -> Fix (Let f) -> m a
letCataM m = phi . unFix 
    where phi x = case x of
                    LetExp a    -> m =<< traverse (letCataM m) a
                    LetBind a e -> letCataM m . e =<< pure a


letCata :: (Functor f) => 
    (f a -> a) -> Fix (Let f) -> a
letCata f = phi . unFix
    where phi x = case x of
                    LetExp a    -> f $ fmap (letCata f) a
                    LetBind a e -> letCata f $ e a


-- ???mix observable and hashconsig
--unsafeCse :: (Ord (f Int), Traversable f) => Fix f -> IO (BiMap (f Int))
--unsafeCse = undefined


hashcons :: (Ord a, Eq a) => a -> State (BiMap a) Int
hashcons e = do
  m <- get
  case lookup_key e m of
    Nothing -> let (k,m') = insert e m
               in  put m' >> return k
    Just k  -> return k




