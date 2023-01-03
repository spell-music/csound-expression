module Csound.Dynamic.Tfm.DeduceTypes(
    Var(..), TypeGraph(..), Convert(..), Stmt, deduceTypes
) where

import Prelude hiding (lines)
import Data.Functor (void)
import Data.List qualified as List
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Traversable as T
import Safe (headMay)

import Data.Set (Set)
import Data.Set qualified as Set
import Data.STRef
import Control.Monad.ST
import Data.Array.ST
import Data.Serialize qualified as Cereal
import GHC.Generics


type TypeRequests s ty = STArray s Int (Set ty)

initTypeRequests :: Int -> ST s (TypeRequests s ty)
initTypeRequests size = newArray (0, size - 1) Set.empty

requestType :: Ord ty => TypeRequests s ty -> Var ty-> ST s ()
requestType arr v = modifyArray arr (varId v) (Set.insert (varType v))

modifyArray :: Ix i => STArray s i a -> i -> (a -> a) -> ST s ()
modifyArray arr i f = writeArray arr i . f =<< readArray arr i

getTypes :: Int -> TypeRequests s ty -> ST s [ty]
getTypes n arr = Set.toList <$> readArray arr n

-- | Typed variable.
data Var a = Var
    { varId   :: !Int
    , varType :: !a
    } deriving (Show, Eq, Ord, Generic)

instance Cereal.Serialize a => Cereal.Serialize (Var a)

data GetType ty
    = NoConversion !ty
    -- If there is a conversion we look for a fresh identifier by map
    -- (map converts mismatched type to fresh identifier)
    | ConversionLookup !(Var ty) !(M.Map ty Int)

type TypeMap ty = IM.IntMap (GetType ty)

lookupVar :: (Show a, Ord a) => TypeMap a -> Var a -> Var a
lookupVar m (Var i r) = case m IM.! i of
    NoConversion     ty        -> Var i ty
    ConversionLookup noConv f  -> maybe noConv (flip Var r) $ M.lookup r f

-- Statement: assignment, like
--    leftHandSide = RightHandSide( arguments )
type Stmt f a = (a, f a)

-- When we have type collisions we have to insert converters:
data Convert ty = Convert
    { convertFrom   :: !(Var ty)
    , convertTo     :: !(Var ty) }

data Line f ty = Line
    { lineId        :: !Int
    , lineGetType   :: !(GetType ty)
    , lineStmt      :: !(Stmt f (Var ty))
    , lineConverts  :: ![Convert ty] }

-- Algorithm specification for the given functor 'f' and type labels of 'a'.
data TypeGraph f ty = TypeGraph
    -- create a type conversion statement
    { mkConvert   :: Convert ty -> Stmt f (Var ty)
    -- for a given statement and a list of requested types for the output produces a pair of
    -- (nonConvertibleTypes, statementWithDeducedTypes)
    -- nonConvertibleTypes is used for insertion of converters.
    , defineType  :: Stmt f Int -> [ty] -> ([ty], Stmt f (Var ty)) }

-- | Deduces types for a dag:
--
-- deduceTypes (functorSpecificFuns) (dag) = (dagWithTypes, lastFreshIdentifier)
--
-- Assumption -- dag is labeled with integers. Labels are unique
-- and a list of labels is a range (0, n) (It's just what we get with CSE algorithm).
--
-- Algorithm proceeds as follows. We init an array of type requests and a reference for fresh identifiers.
-- Type request comes from right hand side of the statement. We need fresh identifiers for converters.
-- If we are going to use a new statement for conversion we need new variables.
--
-- (discussLine)
-- Then we process lines in reverse order and collect type requests by looking at right hand sides
-- and writing type requests for all arguments.
--
-- (processLine)
-- In the second run we substitute all identifiers with typed variables. It's not so strightforward
-- due to converters. If there are converters we have to insert new statements and substitute identifiers
-- with new ones. That's why we convert variables to variables in the processLine.
--
deduceTypes :: forall ty f . (Show ty, Ord ty, T.Traversable f) => TypeGraph f ty -> [Stmt f Int] -> ([Stmt f (Var ty)], Int)
deduceTypes spec as = runST $ do
  freshIds <- newSTRef nextLastIndex
  typeRequests <- initTypeRequests nextLastIndex
  lines <- mapM (discussLine spec typeRequests freshIds) revAs
  let typeMap = toTypeMap lines
  lastId <- readSTRef freshIds
  return (List.foldl' (processLine typeMap) [] lines, lastId)
  where
    toTypeMap :: [Line f ty] -> TypeMap ty
    toTypeMap lines =
      IM.fromList $ map (\Line{..} -> (lineId, lineGetType)) lines

    revAs = reverse as
    nextLastIndex = succ $ maybe 0 fst $ headMay revAs

    processLine :: TypeMap ty -> [Stmt f (Var ty)] -> Line f ty -> [Stmt f (Var ty)]
    processLine !typeMap !res !line =
      ((lhs, fmap (lookupVar typeMap) rhs) : fmap (mkConvert spec) (lineConverts line)) ++ res
      where
        (lhs, rhs) = lineStmt line

discussLine :: forall a f s . (Ord a, T.Traversable f) => TypeGraph f a -> TypeRequests s a -> STRef s Int -> Stmt f Int -> ST s (Line f a)
discussLine spec typeRequests freshIds stmt@(pid, _) = do
  (conv, expr') <- defineType spec stmt <$> getTypes pid typeRequests
  updateTypeRequests expr'
  (getType, convs) <- mkGetType conv (fst expr') freshIds
  return $ Line pid getType expr' convs
  where
    -- update type requests with derived RHS in the statement
    updateTypeRequests :: Stmt f (Var a) -> ST s ()
    updateTypeRequests expr' =
      void $ T.traverse (requestType typeRequests) (snd expr')

mkGetType :: Ord ty => [ty] -> Var ty -> STRef s Int -> ST s (GetType ty, [Convert ty])
mkGetType typesToConvert curVar freshIds
  | null typesToConvert = return (NoConversion $ varType curVar, [])
  | otherwise = do
      ids <- nextIds n freshIds
      return (ConversionLookup curVar $ M.fromList (zip typesToConvert ids),
              zipWith (\i t -> Convert curVar (Var i t)) ids typesToConvert)
    where n = length typesToConvert

nextIds :: Int -> STRef s Int -> ST s [Int]
nextIds n ref = do
    curId <- readSTRef ref
    writeSTRef ref (curId + n)
    return [curId .. n + curId]

