{-# Language TupleSections #-}
module Csound.Dynamic.Tfm.UnfoldMultiOuts(
    unfoldMultiOuts, UnfoldMultiOuts(..), Selector(..)
) where

import Data.List(sortBy)
import Data.Ord(comparing)
import Data.Maybe(mapMaybe, isNothing)
import Control.Monad.Trans.State.Strict
import qualified Data.IntMap as IM

import Csound.Dynamic.Tfm.DeduceTypes(Var(..))

type ChildrenMap = IM.IntMap [Port]

lookupChildren :: ChildrenMap -> Var a -> [Port]
lookupChildren m parentVar = m IM.! varId parentVar

mkChildrenMap :: [(Var a, Selector a)] -> ChildrenMap
mkChildrenMap = IM.fromListWith (++) . fmap extract 
    where extract (var, sel) = (varId $ selectorParent sel, 
                                return $ Port (varId var) (selectorOrder sel))

data Port = Port 
    { portId    :: Int
    , portOrder :: Int } deriving (Show)

type SingleStmt f a = (Var a, f (Var a))
type MultiStmt  f a = ([Var a], f (Var a))

data Selector a = Selector 
    { selectorParent  :: Var a
    , selectorOrder   :: Int }

data UnfoldMultiOuts f a = UnfoldMultiOuts {
    getSelector    :: f (Var a) -> Maybe (Selector a),
    getParentTypes :: f (Var a) -> Maybe [a] }

unfoldMultiOuts :: UnfoldMultiOuts f a -> Int -> [SingleStmt f a] -> ([MultiStmt f a], Int)
unfoldMultiOuts algSpec lastFreshId stmts = runState st lastFreshId
    where selectors = mapMaybe (\(lhs, rhs) -> fmap (lhs,) $ getSelector algSpec rhs) stmts
          st = mapM (unfoldStmt algSpec $ mkChildrenMap selectors) $ dropSelectors stmts
          dropSelectors = filter (isNothing . getSelector algSpec . snd)

unfoldStmt :: UnfoldMultiOuts f a -> ChildrenMap -> SingleStmt f a -> State Int (MultiStmt f a)
unfoldStmt algSpec childrenMap (lhs, rhs) = case getParentTypes algSpec rhs of
    Nothing    -> return ([lhs], rhs)
    Just types -> fmap (,rhs) $ formLhs (lookupChildren childrenMap lhs) types

formLhs :: [Port] -> [a] -> State Int [Var a]
formLhs ports types = fmap (zipWith (flip Var) types) (getPorts ports)
    where getPorts ps = state $ \lastFreshId -> 
            let ps' = sortBy (comparing portOrder) ps
                (ids, lastPortOrder) = runState (mapM (fillMissingPorts lastFreshId) ps') 0
                freshIdForTail = 1 + lastFreshId + inUsePortsSize
                tailIds = map (+ freshIdForTail) [0 .. outputArity - 1 - lastPortOrder]
            in  (concat ids ++ tailIds, lastFreshId + outputArity - inUsePortsSize)

          outputArity = length types    
          inUsePortsSize = length ports  
                                
          fillMissingPorts :: Int -> Port -> State Int [Int]
          fillMissingPorts lastFreshId port = state $ \s ->
                if s == order
                then ([e], next) 
                else (fmap (+ lastFreshId) [s .. order - 1] ++ [e], next)
            where e = portId port
                  order = portOrder port                  
                  next = order + 1

