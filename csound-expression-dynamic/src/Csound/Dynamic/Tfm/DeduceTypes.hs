-- Overview of csound types
-- http://www.csounds.com/journal/issue10/CsoundRates.html
-- http://www.csoundjournal.com/2006spring/controlFlow.html
module Csound.Dynamic.Tfm.DeduceTypes(
  Var(..), TypeGraph(..), Convert(..), Stmt(..), TypeReq(..), fromTypeReq, deduceTypes,
  unifies
) where

import Prelude hiding (lines)
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


type TypeRequests s ty = STArray s Int (Set (TypeReq ty))

initTypeRequests :: Int -> ST s (TypeRequests s ty)
initTypeRequests size = newArray (0, size - 1) Set.empty

saveRequestType :: Ord ty => TypeRequests s ty -> Var (TypeReq ty)-> ST s ()
saveRequestType arr v = modifyArray arr (varId v) (Set.insert (varType v))

modifyArray :: Ix i => STArray s i a -> i -> (a -> a) -> ST s ()
modifyArray arr i f = writeArray arr i . f =<< readArray arr i

getTypes :: TypeRequests s ty -> Int -> ST s [TypeReq ty]
getTypes arr n = Set.toList <$> readArray arr n

-- | Typed variable.
data Var a = Var
    { varId   :: !Int
    , varType :: !a
    } deriving (Show, Eq, Ord, Generic, Functor)

instance Cereal.Serialize a => Cereal.Serialize (Var a)

data GetType ty
    = NoConversion !ty
    -- If there is a conversion we look for a fresh identifier by map
    -- (map converts mismatched type to fresh identifier)
    | ConversionLookup !(Var ty) !(M.Map ty Int)

type TypeMap ty = IM.IntMap (GetType ty)

lookupVar :: (Ord a) => TypeMap a -> Var (TypeReq a) -> Var a
lookupVar m (Var i r) =
  case m IM.! i of
    NoConversion     ty        -> Var i ty
    ConversionLookup noConv f  ->
      if unifies (varType noConv) r
        then noConv
        else maybe noConv (flip Var rTy) $ M.lookup rTy f
  where
    rTy = fromTypeReq r


unifies :: Eq ty => ty -> TypeReq ty -> Bool
unifies rate = \case
  ExactType reqRate  -> rate == reqRate
  AnyTypeOf reqRates -> List.elem rate reqRates

-- Statement: assignment, like
--    leftHandSide = RightHandSide( arguments )
data Stmt f a = Stmt
  { stmtLhs :: !a
  , stmtRhs :: !(f a)
  }
  deriving (Functor, Foldable, Traversable)

-- When we have type collisions we have to insert converters:
data Convert ty = Convert
    { convertFrom   :: !(Var ty)
    , convertTo     :: !(Var ty) }

data Line f ty = Line
    { lineLhs        :: !(Var ty)
    , lineGetType   :: !(GetType ty)
    , lineStmt      :: !(f (Var (TypeReq ty)))
    , lineConverts  :: ![Convert ty]
    }

data TypeReq ty
  = ExactType ty
  | AnyTypeOf [ty]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

fromTypeReq :: Ord ty => TypeReq ty -> ty
fromTypeReq = \case
  ExactType ty -> ty
  AnyTypeOf ty -> maximum ty

-- Algorithm specification for the given functor 'f' and type labels of 'a'.
data TypeGraph f ty = TypeGraph
    -- create a type conversion statement
    { mkConvert   :: Convert ty -> Stmt f (Var ty)
    -- for a given statement and a list of requested types for the output produces a pair of
    -- (nonConvertibleTypes, statementWithDeducedTypes)
    -- nonConvertibleTypes is used for insertion of converters.
    , defineType  :: Var [TypeReq ty] -> f Int -> ([ty], Var ty, f (Var (TypeReq ty))) }

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
deduceTypes :: forall ty f . (Ord ty, T.Traversable f) => TypeGraph f ty -> [Stmt f Int] -> ([Stmt f (Var ty)], Int)
deduceTypes spec as = runST $ do
  freshIds <- newSTRef nextLastIndex
  typeRequests <- initTypeRequests nextLastIndex
  lines <- mapM (discussLine spec typeRequests freshIds) revAs
  let typeMap = toTypeMap lines
  lastId <- readSTRef freshIds
  return (List.foldl' (processLine spec typeMap) [] lines, lastId)
  where
    toTypeMap :: [Line f ty] -> TypeMap ty
    toTypeMap lines =
      IM.fromList $ map (\Line{..} -> (varId lineLhs, lineGetType)) lines

    revAs = reverse as
    nextLastIndex = succ $ maybe 0 stmtLhs $ headMay revAs

processLine ::
     (Functor f, Ord ty)
  => TypeGraph f ty
  -> TypeMap ty -> [Stmt f (Var ty)] -> Line f ty -> [Stmt f (Var ty)]
processLine spec !typeMap !res !line =
  ((Stmt lhs $ fmap (lookupVar typeMap) rhs) : fmap (mkConvert spec) (lineConverts line)) ++ res
  where
    lhs = lineLhs line
    rhs = lineStmt line

discussLine :: forall ty f s . (Ord ty, T.Traversable f)
  => TypeGraph f ty
  -> TypeRequests s ty -> STRef s Int -> Stmt f Int -> ST s (Line f ty)
discussLine spec typeRequests freshIds stmt = do
  outTypeReq <- getTypes typeRequests (stmtLhs stmt)
  let (conv, lhs, rhs) = defineType spec (Var (stmtLhs stmt) outTypeReq) (stmtRhs stmt)
  updateTypeRequests rhs
  (getType, convs) <- mkGetType conv lhs freshIds
  return $ Line lhs getType rhs convs
  where
    -- update type requests with derived RHS in the statement
    updateTypeRequests :: f (Var (TypeReq ty)) -> ST s ()
    updateTypeRequests expr =
      mapM_ (saveRequestType typeRequests) expr

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

