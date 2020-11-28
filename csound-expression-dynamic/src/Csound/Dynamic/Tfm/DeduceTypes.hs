module Csound.Dynamic.Tfm.DeduceTypes(
    Var(..), TypeGraph(..), Convert(..), Stmt, deduceTypes    
) where

import Data.List(nub)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Traversable as T

import Data.STRef
import Control.Monad.ST
import Data.Array.ST

type TypeRequests s ty = STArray s Int [ty]

initTypeRequests :: Int -> ST s (TypeRequests s ty)
initTypeRequests size = newArray (0, size - 1) []

requestType :: Var ty -> TypeRequests s ty -> ST s ()
requestType v arr = modifyArray arr (varId v) (varType v :)

modifyArray :: Ix i => STArray s i a -> i -> (a -> a) -> ST s ()
modifyArray arr i f = writeArray arr i . f =<< readArray arr i

getTypes :: Int -> TypeRequests s ty -> ST s [ty]
getTypes n arr = readArray arr n

-- | Typed variable.
data Var a = Var 
    { varId   :: Int
    , varType :: a 
    } deriving (Show, Eq, Ord)

data GetType ty     
    = NoConversion ty 
    -- If there is a conversion we look for a fresh identifier by map 
    -- (map converts mismatched type to fresh identifier)
    | ConversionLookup (Var ty) (M.Map ty Int)

type TypeMap ty = IM.IntMap (GetType ty)

lookupVar :: (Show a, Ord a) => TypeMap a -> Var a -> Var a
lookupVar m (Var i r) = case m IM.! i of
    NoConversion     ty        -> Var i ty
    ConversionLookup noConv f  -> maybe noConv (flip Var r) $ M.lookup r f 

-- Statement: assignment, like
--    leftHandSide = RightHandSide( arguments )
type Stmt f a = (a, f a)

-- When we haave type collisions we have to insert converters:
data Convert a = Convert
    { convertFrom   :: Var a
    , convertTo     :: Var a }

data Line f a = Line 
    { lineType      :: (Int, GetType a) 
    , lineStmt      :: Stmt f (Var a) 
    , lineConverts  :: [Convert a] }

-- Algorithm specification for the given functor 'f' and type labels of 'a'.
data TypeGraph f a = TypeGraph 
    -- create a type conversion statement
    { mkConvert   :: Convert a -> Stmt f (Var a)
    -- for a given statement and a list of requested types for the output produces a pair of
    -- (nonConvertibleTypes, statementWithDeducedTypes)
    -- nonConvertibleTypes is used for insertion of converters.
    , defineType  :: Stmt f Int -> [a] -> ([a], Stmt f (Var a)) }

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
-- In the second run we substitute all identifiers with typed variables. It's no so strightforward
-- due to converters. If there are converters we have to insert new statements and substitute identifiers
-- with new ones. That's why we convert variables to variables in the processLine. 
--
deduceTypes :: (Show a, Ord a, T.Traversable f) => TypeGraph f a -> [Stmt f Int] -> ([Stmt f (Var a)], Int)
deduceTypes spec as = runST $ do
    freshIds <- newSTRef n
    typeRequests <- initTypeRequests n
    lines' <- mapM (discussLine spec typeRequests freshIds) $ reverse as
    let typeMap = IM.fromList $ fmap lineType lines'
    lastId <- readSTRef freshIds
    return (reverse $ processLine typeMap =<< lines', lastId)
    where n = succ $ if (null as) then 0 else (fst $ last as)
          processLine typeMap line = fmap (mkConvert spec) (lineConverts line) ++ [(a, fmap (lookupVar typeMap) b)]
              where (a, b) = lineStmt line            

discussLine :: (Ord a, T.Traversable f) => TypeGraph f a -> TypeRequests s a -> STRef s Int -> Stmt f Int -> ST s (Line f a)
discussLine spec typeRequests freshIds stmt@(pid, _) = do
    (conv, expr') <- fmap (defineType spec stmt . nub) $ getTypes pid typeRequests
    _ <- T.traverse (flip requestType typeRequests) (snd expr')
    let curType = fst expr'
    (getType, convs) <- mkGetType conv curType freshIds
    return $ Line (pid, getType) expr' convs

mkGetType :: Ord a => [a] -> Var a -> STRef s Int -> ST s (GetType a, [Convert a])
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

