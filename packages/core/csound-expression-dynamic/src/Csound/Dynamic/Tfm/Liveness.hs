{- | Analyses variable for reusal.
So that we spend less memory and allocate lesser variables and audio vectors.
-}
module Csound.Dynamic.Tfm.Liveness (
  liveness,
) where

import Prelude hiding (mapM, mapM_)

import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.Traversable

import Control.Monad hiding (mapM, mapM_)
import Control.Monad.ST
import Control.Monad.Trans.Class
import Data.Vector.Unboxed.Mutable qualified as UVector

import Csound.Dynamic.Tfm.InferTypes (Var (..))
import Csound.Dynamic.Types.Exp (Rate (..))

{- | Reuses variables. It analyses weather the vraibel is used further
in the code and if it's not used it tries to reuse it for the next assignments
-}
liveness :: (Traversable f) => Int -> Dag f -> Dag f
liveness lastFreshId as = runST $ do
  st <- initSt lastFreshId =<< analyse lastFreshId as
  evalStateT (mapM substExp $ countLines $ as) st

type LineNumber = Int

countLines :: [a] -> [(LineNumber, a)]
countLines = zip [0 ..]

type Lhs = [Var]
type Rhs f = f Var
type Exp f = (Lhs, Rhs f)
type Dag f = [Exp f]

-----------------------------------------------

data IdList
  = IdList
      [Int] -- fresh ids (always infinite list)
      !Int -- the biggest used id

allocId :: IdList -> (Int, IdList)
allocId (IdList is lastId) =
  case is of
    hd : tl -> (hd, IdList tl (max hd lastId))
    [] -> error "impossible: list of IDs is always infinite"

freeId :: Int -> IdList -> IdList
freeId n (IdList is lastId) = IdList (insertSorted n is) lastId1
  where
    lastId1 = if (n == lastId) then (lastId - 1) else lastId

insertSorted :: Int -> [Int] -> [Int]
insertSorted n (a : as)
  | n < a = n : a : as
  | n == a = a : as
  | otherwise = a : insertSorted n as
insertSorted n [] = [n]

initIdList :: IdList
initIdList = IdList [0 ..] 0

-----------------------------------------------

type StArr s = UVector.STVector s Int

type LivenessTable s = UVector.STVector s Int
type SubstTable s = StArr s

data Registers s = Registers
  { arRegisters :: !IdList
  , krRegisters :: !IdList
  , livenessTable :: !(LivenessTable s)
  , substTable :: !(SubstTable s)
  }

type Memory s a = StateT (Registers s) (ST s) a

onRegs :: Rate -> (IdList -> IdList) -> Memory s ()
onRegs rate f = modify' $ \rs ->
  case rate of
    Ar -> rs{arRegisters = f $ arRegisters rs}
    Kr -> rs{krRegisters = f $ krRegisters rs}
    _ -> rs

setArRegisters :: IdList -> Memory s ()
setArRegisters ids = modify' $ \s -> s{arRegisters = ids}

setKrRegisters :: IdList -> Memory s ()
setKrRegisters ids = modify' $ \s -> s{krRegisters = ids}

isAlive :: LineNumber -> Var -> Memory s Bool
isAlive lineNum v = do
  tab <- fmap livenessTable get
  lastUsage <- UVector.read tab (varId v)
  pure $ lineNum < lastUsage

lookUpSubst :: Int -> Memory s Int
lookUpSubst i = do
  tab <- fmap substTable get
  lift $ UVector.read tab i

saveSubst :: Int -> Int -> Memory s ()
saveSubst from to = do
  tab <- fmap substTable get
  lift $ UVector.write tab from to

substLhs :: Var -> Memory s Var
substLhs = onlyForAK $ \v -> do
  v1 <- alloc v
  saveSubst (varId v) (varId v1)
  return v1

substRhs :: LineNumber -> Var -> Memory s Var
substRhs lineNum = onlyForAK $ \v -> do
  i1 <- lookUpSubst (varId v)
  let
    v1 = Var (varType v) i1
  b <- isAlive lineNum v
  unless b $ free v1
  return v1

alloc :: Var -> Memory s Var
alloc v =
  case varType v of
    Ar -> allocBy arRegisters setArRegisters
    Kr -> allocBy krRegisters setKrRegisters
    _ -> pure v
  where
    allocBy extract update = do
      ids <- gets extract
      let
        (name, newIds) = allocId ids
      void $ update newIds
      pure (Var (varType v) name)

free :: Var -> Memory s ()
free (Var rate name) = onRegs rate (freeId name)

--------------------------------------------------------------------------

analyse :: (Traversable f) => Int -> Dag f -> ST s (LivenessTable s)
analyse lastFreshId as = do
  arr <- UVector.replicate lastFreshId 0
  mapM_ (go arr) $ countLines as
  return arr
  where
    go :: (Traversable f) => StArr s -> (LineNumber, Exp f) -> ST s ()
    go arr (lineNum, (_, rhs)) = mapM_ (countVar arr lineNum) rhs

    countVar :: StArr s -> LineNumber -> Var -> ST s ()
    countVar arr lineNum v
      | isAOrK v = UVector.write arr (varId v) lineNum
      | otherwise = pure ()

onlyForAK :: (Monad f) => (Var -> f Var) -> Var -> f Var
onlyForAK go v
  | isAOrK v = go v
  | otherwise = pure v

-- we optimise for livenes only for Ar and Kr variables
isAOrK :: Var -> Bool
isAOrK v =
  case varType v of
    Ar -> True
    Kr -> True
    _ -> False

substExp :: (Traversable f) => (LineNumber, Exp f) -> Memory s (Exp f)
substExp (lineNum, (lhs, rhs)) = do
  freshLhs <- traverse substLhs lhs
  freshRhs <- traverse (substRhs lineNum) rhs
  return (freshLhs, freshRhs)

initSt :: Int -> LivenessTable s -> ST s (Registers s)
initSt lastFreshId livenessTab = fmap (Registers initIdList initIdList livenessTab) (initSubstTable lastFreshId)

initSubstTable :: Int -> ST s (SubstTable s)
initSubstTable n = UVector.generate (n + 1) id
