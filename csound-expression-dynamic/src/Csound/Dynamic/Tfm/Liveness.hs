-- | Analyses variable for reusal.
-- So that we spend less memory and allocate lesser variables and audio vectors.
module Csound.Dynamic.Tfm.Liveness (
    liveness
) where

import Prelude hiding (mapM, mapM_)

import Control.Monad.Trans.State.Strict
import Data.Traversable
import Data.Foldable
import qualified Data.Map.Strict as M

import Control.Monad.Trans.Class
import Control.Monad hiding (mapM, mapM_)
import Control.Monad.ST
import qualified Data.Array.Unboxed as A
import qualified Data.Array.MArray as A
import qualified Data.Array.ST as A

import Csound.Dynamic.Tfm.InferTypes (Var (..))
import Csound.Dynamic.Types.Exp(Rate(..))

-- | Reuses variables. It analyses weather the vraibel is used further
-- in the code and if it's not used it tries to reuse it for the next assignments
liveness :: Traversable f => Int -> Dag f -> Dag f
liveness lastFreshId as = runST $ do
  st <- initSt lastFreshId $ analyse lastFreshId as
  evalStateT (mapM substExp $ countLines $ as) st

type LineNumber = Int

countLines :: [a] -> [(LineNumber, a)]
countLines = zip [0 ..]

type Lhs   = [Var]
type Rhs f = f Var
type Exp f = (Lhs, Rhs f)

type Dag f = [Exp f]

-----------------------------------------------

newtype IdList = IdList [Int] -- fresh ids

initIdList :: IdList
initIdList = IdList [0..]

allocId :: IdList -> (Int, IdList)
allocId (IdList ids) =
  case ids of
    a:as -> (a, IdList as)
    []    -> error "Can not be empty"

freeId :: Int -> IdList -> IdList
freeId a (IdList as) = IdList (a:as)

-----------------------------------------------

type StArr s = A.STUArray s Int Int

type LivenessTable = A.UArray Int Int
type SubstTable s  = StArr s

data Registers s = Registers
  { registers     :: !(M.Map Rate IdList)
  , livenessTable :: !LivenessTable
  , substTable    :: !(SubstTable s)
  }

type Memory s a = StateT (Registers s) (ST s) a

onRegs ::  (M.Map Rate IdList -> M.Map Rate IdList) -> (Registers s -> Registers s)
onRegs f rs = rs { registers = f $ registers rs }

initRegs :: M.Map Rate IdList
initRegs = M.fromList $ fmap (\x -> (x, initIdList)) [(minBound :: Rate) .. maxBound]

isAlive :: LineNumber -> Var -> Memory s Bool
isAlive lineNum v = do
  tab <- fmap livenessTable get
  return $ lineNum < tab A.! (varId v)

lookUpSubst :: Int -> Memory s Int
lookUpSubst i = do
  tab <- fmap substTable get
  lift $ A.readArray tab i

saveSubst :: Int -> Int -> Memory s ()
saveSubst from to = do
  tab <- fmap substTable get
  lift $ A.writeArray tab from to

substLhs :: Var -> Memory s Var
substLhs v = do
  v1 <- allocAndSkipInits v
  saveSubst (varId v) (varId v1)
  return v1

substRhs :: LineNumber -> Var -> Memory s Var
substRhs lineNum v = do
  i1 <- lookUpSubst (varId v)
  let v1 = Var (varType v) i1
  b <- isAlive lineNum v
  unless b $ free v1
  return v1

allocAndSkipInits :: Var -> Memory s Var
allocAndSkipInits v
    | isInit r  = return v
    | otherwise = alloc r
    where
        r = varType v
        isInit x = x == Ir || x == Sr

alloc :: Rate -> Memory s Var
alloc rate = state $ \mem ->
  let (i, mem1) = allocRegister rate mem
  in  (Var rate i, mem1)
  where
    allocRegister :: Rate -> Registers s -> (Int, Registers s)
    allocRegister r mem = (i, onRegs (M.update (const $ Just is) r) mem)
      where (i, is) = allocId $ registers mem M.! r

free :: Var -> Memory s ()
free v = state $ \mem ->
  let mem1 = freeRegister (varType v) (varId v) mem
  in  ((), mem1)
  where
    freeRegister :: Rate -> Int -> Registers s -> Registers s
    freeRegister rate i = onRegs $ M.update (Just . freeId i) rate

--------------------------------------------------------------------------

analyse :: Traversable f => Int -> Dag f -> LivenessTable
analyse lastFreshId as = A.runSTUArray $ do
  arr <- A.newArray (0, lastFreshId) 0
  mapM_ (go arr) $ countLines as
  return arr
  where
    go :: Traversable f => StArr s -> (LineNumber, Exp f) -> ST s ()
    go arr (lineNum, (_, rhs)) =  mapM (countVar arr lineNum) rhs >> return ()

    countVar :: StArr s  -> LineNumber -> Var -> ST s ()
    countVar arr lineNum v = do
      val <- A.readArray arr i
      A.writeArray arr i (val `max` lineNum)
      where i = varId v

substExp :: Traversable f => (LineNumber, Exp f) -> Memory s (Exp f)
substExp (lineNum, (lhs, rhs)) = do
  freshLhs <- traverse substLhs lhs
  freshRhs <- traverse (substRhs lineNum) rhs
  return (freshLhs, freshRhs)

initSt :: Int -> LivenessTable -> ST s (Registers s)
initSt lastFreshId livenessTab = fmap (Registers initRegs livenessTab) (initSubstTable lastFreshId)

initSubstTable :: Int ->  ST s (SubstTable s)
initSubstTable n = A.newListArray (0, n+1) [0 .. n + 1]

