{-# Language TupleSections #-}
module Csound.Dynamic.Tfm.UnfoldMultiOuts(
  unfoldMultiOuts, Selector(..)
) where

import Data.List(sortBy)
import Data.Ord(comparing)
import Data.Maybe(mapMaybe, isNothing)
import Control.Monad.Trans.State.Strict
import qualified Data.IntMap.Strict as IM

import Csound.Dynamic.Tfm.InferTypes(Var(..), Stmt(..), InferenceResult(..))
import Csound.Dynamic.Types.Exp hiding (Var (..))
import Csound.Dynamic.Build(getRates, isMultiOutSignature)

type ChildrenMap = IM.IntMap [Port]

lookupChildren :: ChildrenMap -> Var -> [Port]
lookupChildren m parentVar = m IM.! varId parentVar

mkChildrenMap :: [(Var, Selector)] -> ChildrenMap
mkChildrenMap = IM.fromListWith (++) . fmap extract
    where extract (var, sel) = (varId $ selectorParent sel,
                                return $ Port (varId var) (selectorOrder sel))

data Port = Port
    { portId    :: Int
    , portOrder :: Int } deriving (Show)

type SingleStmt = Stmt Var
type MultiStmt  = ([Var], RatedExp Var)

data Selector = Selector
    { selectorParent  :: Var
    , selectorOrder   :: Int
    }

unfoldMultiOuts :: InferenceResult -> ([MultiStmt], Int)
unfoldMultiOuts InferenceResult{..} = runState st programLastFreshId
    where
      selectors = mapMaybe (\(Stmt lhs rhs) -> fmap (lhs, ) $ getSelector rhs) typedProgram
      st = mapM (unfoldStmt $ mkChildrenMap selectors) $ dropSelectors typedProgram
      dropSelectors = filter (isNothing . getSelector . stmtRhs)

unfoldStmt :: ChildrenMap -> SingleStmt -> State Int MultiStmt
unfoldStmt childrenMap (Stmt lhs rhs) = case getParentTypes rhs of
    Nothing    -> return ([lhs], rhs)
    Just types -> fmap (,rhs) $ formLhs (lookupChildren childrenMap lhs) types

formLhs :: [Port] -> [Rate] -> State Int [Var]
formLhs ports types = fmap (zipWith Var types) (getPorts ports)
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

-----------------------------------------------------------------------
-- unfolds multiple rates generic functions

getSelector :: RatedExp Var -> Maybe Selector
getSelector x =
  case ratedExpExp x of
    Select _ order (PrimOr (Right parent)) -> Just $ Selector parent order
    _ -> Nothing

getParentTypes :: RatedExp Var -> Maybe [Rate]
getParentTypes x =
  case ratedExpExp x of
    Tfm i _ -> if (isMultiOutSignature $ infoSignature i)
                then Just (getRates $ ratedExpExp x)
                else Nothing
    _ -> Nothing
