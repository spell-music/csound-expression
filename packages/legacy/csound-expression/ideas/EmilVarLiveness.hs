{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.State
import Data.Foldable (Foldable)
import qualified Data.Foldable as Fold
import Data.Map (Map)
import qualified Data.Map as Map



----------------------------------------------------------------------------------------------------
-- Algorithm for reusing variables
----------------------------------------------------------------------------------------------------

-- | Assignment
data a := b = a := b
  deriving (Eq, Show)

listProg :: (Show a, Show b) => [a := b] -> IO ()
listProg = mapM_ print

-- | Variable liveness
type Liveness a = Map a Int

-- | Variable aliasing
type Alias a = Map a a

-- | Count the number of times each variable is referenced
countRefs :: (Ord a, Foldable f) => [a := f a] -> Liveness a
countRefs prog = Map.fromListWith (+) [(b,1) | a := f <- prog, b <- a : Fold.toList f]

-- | Decrease the ref count for the given variable
decRef :: (Ord a) => a -> State (Liveness a, Alias a) ()
decRef ref = do
    (live,alias) <- get
    let live' = Map.alter (fmap (+ (-1))) ref live
    put (live',alias)

-- | Decrease the ref count for each variable in the given expression
updateLiveness :: (Ord a, Foldable f) => a := f a -> State (Liveness a, Alias a) ()
updateLiveness (a := f) = decRef a >> mapM_ decRef (Fold.toList f)

-- | Find a dead variable
findDeadVar :: (Eq a) => State (Liveness a, Alias a) (Maybe a)
findDeadVar = do
    (live,alias) <- get
    let dead = Map.assocs $ Map.filter (==0) live
    return $ case dead of
        [] -> Nothing
        (a,_):_ -> Just a
  -- TODO Improve the linear complexity by remembring the set of dead vars in the state

-- | Get the alias of a variable
getAlias :: (Ord a) => Map a a -> a -> a
getAlias alias a = Map.findWithDefault a a alias

-- | Update an expression according to the available aliases
aliasExpr :: (Ord a, Functor f) => f a -> State (Liveness a, Alias a) (f a)
aliasExpr f = do
    (live,alias) <- get
    return (fmap (getAlias alias) f)

-- | Make @old@ and alias for @new@
reuseVar :: (Ord a) => a -> a -> State (Liveness a, Alias a) ()
reuseVar old new = do
    (live,alias) <- get
    let live'  = Map.insert old (live Map.! new) live
        alias' = Map.insert new old alias
    put (live',alias')

reuseVarsM :: (Ord a, Functor f, Foldable f) => [a := f a] -> State (Liveness a, Alias a) [a := f a]
reuseVarsM prog = forM prog $ \(a := f) -> do
    g    <- aliasExpr f
    dead <- findDeadVar
    updateLiveness (a := g)
    case dead of
        Nothing -> return (a := g)
        Just b  -> reuseVar b a >> return (b := g)

-- | Optimize the program by reusing dead variables
reuseVars :: (Ord a, Functor f, Foldable f) => [a := f a] -> [a := f a]
reuseVars prog = evalState (reuseVarsM prog) (countRefs prog, Map.empty)



----------------------------------------------------------------------------------------------------
-- Verification
----------------------------------------------------------------------------------------------------

class Eval f
  where
    eval :: f Int -> Int

executeAssign :: (Ord a, Functor f, Eval f) => a := f a -> State (Map a Int) ()
executeAssign (a := f) = modify $ \mem -> Map.insert a (eval $ fmap (mem Map.!) f) mem

execute :: (Ord a, Functor f, Eval f) => [a := f a] -> Int
execute prog = mem Map.! res
  where
    res := _ = last prog
    mem      = execState (mapM_ executeAssign prog) Map.empty

prop_reuseVars prog = execute prog == execute (reuseVars prog)



----------------------------------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------------------------------

data E a
    = Int Int
    | Add a a
  deriving (Eq, Show, Functor, Foldable)

instance Eval E
  where
    eval (Int a)   = a
    eval (Add a b) = a + b

prog1 =
    [ "x" := Int 4
    , "y" := Int 5
    , "z" := Add "x" "y"
    , "a" := Add "z" "z"
    , "b" := Add "a" "a"
    , "c" := Add "z" "x"
    ]

test1 = countRefs prog1
test2 = listProg $ reuseVars prog1
test3 = prop_reuseVars prog1

