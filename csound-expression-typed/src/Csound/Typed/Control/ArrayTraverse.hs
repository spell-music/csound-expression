{-# Language ScopedTypeVariables #-}
-- | Array traversals and folds
module Csound.Typed.Control.ArrayTraverse(        
    foreachArr, foreachArrD, forRowArr, forColumnArr, forRowArrD, forColumnArrD,
    foldArr, foldRowArr, foldColumnArr, foldRowsArrD, foldColumnsArrD
) where

import Csound.Typed.Types
import Csound.Typed.Control.Ref
import Csound.Typed.GlobalState
import Data.Boolean
import qualified Csound.Dynamic as D  

-------------------------------------------------------------------------
-- Functional style traversals

whileRefBegin :: SigOrD a => Ref a -> SE ()
whileRefBegin (Ref vars) = fromDep_ $ D.whileRef $ head vars

-- | Traverses all elements of the array array and applies a procedure to each element.
-- The procedure takes in a pair of index and the current value at the given index.
foreachArr :: (Tuple ix, Tuple a) => Arr ix a -> ((ix, a) -> SE ()) -> SE ()
foreachArr = foreachArrBy getArrayLength
    where
        getArrayLength :: Int -> Arr ix a -> Sig
        getArrayLength n array = lenarray array `withD` (int n)

-- | Traverses all elements of the array at the **init rate** and applies a procedure to each element.
-- The procedure takes in a pair of index and the current value at the given index.
foreachArrD :: (Tuple ix, Tuple a) => Arr ix a -> ((ix, a) -> SE ()) -> SE ()
foreachArrD = foreachArrBy getArrayLength
    where
        getArrayLength :: Int -> Arr ix a -> D
        getArrayLength n array = lenarray array `withD` (int n)

foreachArrBy :: forall a b ix . (OrdB b, IfB b, Num b, SigOrD b, Tuple b, Tuple ix, Tuple a) => (Int -> Arr ix a -> b) ->  Arr ix a -> ((ix, a) -> SE ()) -> SE ()
foreachArrBy getArrayLength array body = do
    vars <- mapM newCtrlRef $ replicate arity (0 :: b)
    condVars <- mapM newCtrlRef $ replicate arity (1 :: b)
    recWhile vars $ zip3 [1 ..] vars condVars
    where
        recWhile :: [Ref b] -> [(Int, Ref b, Ref b)] -> SE ()
        recWhile vars xs = case xs of
            [] -> do
                ix <- readRef $ concatRef vars
                val <- readArr array ix
                body (ix, val)
            (n, var, condVar) : rest -> do
                whileRefBegin condVar 

                recWhile vars rest

                modifyRef var (+ 1)
                ix <- readRef var                
                writeRef condVar (ifB (ix `lessThan` getArrayLength n array) 1 0)

                fromDep_ D.whileEnd

        arity = tupleArity $ proxy array

        proxy :: Arr ix a -> ix
        proxy = const undefined

        concatRef :: [Ref b] -> Ref ix
        concatRef vs = Ref $ vs >>= \(Ref xs) -> xs

-- | Traverses all elements in the given row of 2D array at the signal rate and applies a procedure to all elements.
forRowArr :: (Tuple a) => Sig -> Arr Sig2 a -> ((Sig, a) -> SE ()) -> SE ()
forRowArr rowId array phi = whileRef 0 cond body
    where
        cond ix = return $ ix `lessThan` lenarray array `withD` 2

        body ix = do
            val <- readArr array (rowId, ix)
            phi (ix, val)
            return $ ix + 1


-- | Traverses all elements in the given column of 2D array at the signal rate and applies a procedure to all elements.
forColumnArr :: (Tuple a) => Sig -> Arr Sig2 a -> ((Sig, a) -> SE ()) -> SE ()
forColumnArr colId array phi = whileRef 0 cond body
    where
        cond ix = return $ ix `lessThan` lenarray array `withD` 1

        body ix = do
            val <- readArr array (ix, colId)
            phi (ix, val)
            return $ ix + 1

-- | Traverses all elements in the given row of 2D array at the init rate and applies a procedure to all elements.
forRowArrD :: Tuple a => D -> Arr D2 a -> ((D, a) -> SE ()) -> SE () 
forRowArrD rowId array phi = whileRefD 0 cond body
    where
        cond ix = return $ ix `lessThan` lenarray array `withD` 2

        body ix = do
            val <- readArr array (rowId, ix)
            phi (ix, val)
            return $ ix + 1

-- | Traverses all elements in the given column of 2D array at the init rate and applies a procedure to all elements.
forColumnArrD :: Tuple a => D -> Arr D2 a -> ((D, a) -> SE ()) -> SE ()
forColumnArrD colId array phi = whileRefD 0 cond body
    where
        cond ix = return $ ix `lessThan` lenarray array `withD` 1

        body ix = do
            val <- readArr array (ix, colId)
            phi (ix, val)
            return $ ix + 1

-- | Traverses an array and accumulates a value. We invoke the function with accumulator function, initial value and the array.
foldArr :: (Tuple ix, Tuple a, Tuple b) => ((ix, a) -> b -> SE b) -> b -> Arr ix a -> SE b
foldArr phi z array = do
    res <- newRef z
    foreachArr array (toFoldFun phi res)
    readRef res

toFoldFun :: Tuple b => (a -> b -> SE b) -> Ref b -> a -> SE ()
toFoldFun phi ref a = writeRef ref =<< phi a =<< readRef ref

-- | Traverses a row in the array and accumulates a value. We invoke the function 
-- with accumulator function, initial value and the array with signal of the row number.
--
-- > foldRowArr accum initValue rowId array
foldRowArr :: (Tuple a, Tuple b) => ((Sig, a) -> b -> SE b) -> b -> Sig -> Arr Sig2 a -> SE b
foldRowArr phi z rowId array = do
    res <- newRef z
    forRowArr rowId array $ toFoldFun phi res
    readRef res

-- | Traverses a column in the array and accumulates a value. We invoke the function 
-- with accumulator function, initial value and the array with signal of the row number.
--
-- > foldColumnArr accum initValue columnId array
foldColumnArr :: (Tuple a, Tuple b) => ((Sig, a) -> b -> SE b) -> b -> Sig -> Arr Sig2 a -> SE b
foldColumnArr phi z rowId array = do
    res <- newRef z
    forColumnArr rowId array $ toFoldFun phi res
    readRef res

-- | Traverses a row at the **init rate** in the array and accumulates a value. We invoke the function 
-- with accumulator function, initial value and the array with signal of the row number.
--
-- > foldRowArr accum initValue rowId array
foldRowsArrD :: (Tuple a, Tuple b) => ((D, a) -> b -> SE b) -> b -> D -> Arr D2 a -> SE b
foldRowsArrD phi z rowId array = do
    res <- newRef z
    forRowArrD rowId array $ toFoldFun phi res
    readRef res

-- | Traverses a column at the **init rate** in the array and accumulates a value. We invoke the function 
-- with accumulator function, initial value and the array with signal of the row number.
--
-- > foldColumnArr accum initValue columnId array
foldColumnsArrD :: (Tuple a, Tuple b) => ((D, a) -> b -> SE b) -> b -> D -> Arr D2 a -> SE b
foldColumnsArrD phi z rowId array = do
    res <- newRef z
    forColumnArrD rowId array $ toFoldFun phi res
    readRef res
