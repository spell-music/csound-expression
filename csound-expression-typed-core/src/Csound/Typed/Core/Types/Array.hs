{-# Language FlexibleInstances, ScopedTypeVariables #-}
module Csound.Typed.Core.Types.Array(
    Arr(..),
    newLocalArr, newGlobalArr, newLocalCtrlArr, newGlobalCtrlArr,
    fillLocalArr, fillGlobalArr, fillLocalCtrlArr, fillGlobalCtrlArr,
    readArr, writeArr, writeInitArr, modifyArr, mixArr,
    -- * Misc functions to help the type inverence
    Arr1, DArr1, Arr2, DArr2, Arr3, DArr3,
    arr1, darr1, arr2, darr2, arr3, darr3,

    -- * Array opcodes
    maparrayNew, lenarray, copyf2array, copya2ftab, minarray, maxarray, sumarray,
    scalearray, slicearrayNew,

    maparrayCopy, slicearrayCopy,
    divArrayCopy, subArrayCopy,
    addArrayCopy, mulArrayCopy,
    addArrayNew, mulArrayNew, divArrayNew, subArrayNew,

    -- * Spectral opcodes
    SpecArr,

    fftNew, fftinvNew, rfftNew, rifftNew, pvs2tab, tab2pvs, cmplxprodNew,
    rect2polNew, pol2rectNew, pol2rect2New, windowArrayNew,
    r2cNew, c2rNew, magsArrayNew, phsArrayNew,

    fftCopy, fftinvCopy, rfftCopy, rifftCopy, cmplxprodCopy,
    rect2polCopy, pol2rectCopy, pol2rect2Copy, windowArrayCopy,
    r2cCopy, c2rCopy, magsArrayCopy, phsArrayCopy
) where


import Control.Monad
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)

import Csound.Dynamic (E, Rate (..), Var, DepT)
import qualified Csound.Dynamic as D

import Csound.Typed.Core.Types.Prim
import Csound.Typed.Core.Types.Tuple
import Csound.Typed.Core.Types.SE
import Csound.Typed.Core.State (Run)
import Csound.Typed.Core.State qualified as State

------------------------------------------------------------------
-- allocation of array vars

newGlobalArrVar :: Rate -> Run [E] -> Dep Var
newGlobalArrVar r v = lift $ State.initGlobalArrVar r =<< v

type Dep a = DepT Run a

-- | An array with single signal index.
type Arr1 a  = Arr Sig a

-- | An array with single constant index.
type DArr1 a = Arr D   a


-- | A matrix (2D array) with signal index.
type Arr2 a  = Arr (Sig, Sig) a

-- | A matrix (2D array) with constant index.
type DArr2 a = Arr (D, D) a

-- | A 3D array with signal index.
type Arr3 a  = Arr (Sig, Sig, Sig) a

-- | A 3D array with constant index.
type DArr3 a = Arr (D, D, D) a

-- | Function to help the type inference.
arr1  :: SE (Arr Sig a) -> SE (Arr Sig a)
arr1 = id

-- | Function to help the type inference.
darr1 :: SE (Arr D   a) -> SE (Arr D   a)
darr1 = id

-- | Function to help the type inference.
arr2  :: SE (Arr (Sig,Sig) a) -> SE (Arr (Sig,Sig) a)
arr2 = id

-- | Function to help the type inference.
darr2 :: SE (Arr (D,D)   a)   -> SE (Arr (D,D)     a)
darr2 = id

-- | Function to help the type inference.
arr3  :: SE (Arr (Sig,Sig,Sig) a) -> SE (Arr (Sig,Sig,Sig) a)
arr3 = id

-- | Function to help the type inference.
darr3 :: SE (Arr (D,D,D)   a)     -> SE (Arr (D,D,D)     a)
darr3 = id

-- | Arrays. The array data type is parametrized with type of the index and the type of the value.
-- Note that the data tpyes for indices and values can be tuples.
newtype Arr ix a = Arr { unArr :: [Var] }

newArrBy :: forall ix a . (Tuple a, Tuple ix) => (Rate -> Run [E] -> Dep Var) -> [D] -> SE (Arr ix a)
newArrBy mkVar sizes =
    fmap Arr $ mapM (\x -> SE $ mkVar x (mapM toE sizes)) (tupleRates @a)

getIndices :: Tuple ix => [Int] -> [ix]
getIndices xs = fmap (toTuple . return . fmap D.int) $ getIntIndices xs

getIntIndices :: [Int] -> [[Int]]
getIntIndices = fmap reverse . foldl go []
    where
        go :: [[Int]] -> Int -> [[Int]]
        go res n = case res of
            [] -> fmap (\x -> [x]) ix
            xs -> [ first : rest | first <- ix, rest <- xs ]
            where ix = [0 .. n - 1]

fillArrBy :: (Tuple a, Tuple ix) => (Rate -> Run [E] -> Dep Var) -> [Int] -> [a] -> SE (Arr ix a)
fillArrBy mkVar sizes inits = do
    arr <- newArrBy mkVar (fmap int sizes)
    zipWithM_  (writeInitArr arr) (getIndices sizes) inits
    return arr

-- | Creates an array that is local to the body of Csound instrument where it's defined.
-- The array contains audio signals.
--
-- > newLocalArr sizes
newLocalArr :: (Tuple a, Tuple ix) => [D] -> SE (Arr ix a)
newLocalArr = newArrBy D.newLocalArrVar

-- | Creates a global array. The array contains audio signals.
--
-- > newGlobalArr sizes
newGlobalArr :: (Tuple a, Tuple ix) => [D] -> SE (Arr ix a)
newGlobalArr = newArrBy newGlobalArrVar

-- | Creates an array that is local to the body of Csound instrument where it's defined.
-- The array contains control signals.
--
-- > newLocalCtrlArr sizes
newLocalCtrlArr :: (Tuple a, Tuple ix) => [D] -> SE (Arr ix a)
newLocalCtrlArr = newArrBy newLocalCtrlArrVar

-- | Creates a global array. The array contains control signals.
--
-- > newGlobalCtrlArr sizes
newGlobalCtrlArr :: (Tuple a, Tuple ix) => [D] -> SE (Arr ix a)
newGlobalCtrlArr = newArrBy newGlobalCtrlArrVar

-- | Creates an array that is local to the body of Csound instrument where it's defined.
-- The array contains audio signals. It fills the array from the list of values (the last argument).
--
-- > fillLocalArr sizes initValues = ...
fillLocalArr :: (Tuple a, Tuple ix) => [Int] -> [a] -> SE (Arr ix a)
fillLocalArr = fillArrBy D.newLocalArrVar

-- | Creates a global array. The array contains audio signals. It fills the array from the list of values (the last argument).
--
-- > fillGlobalArr sizes initValues = ...
fillGlobalArr :: (Tuple a, Tuple ix) => [Int] -> [a] -> SE (Arr ix a)
fillGlobalArr = fillArrBy newGlobalArrVar

-- | Creates an array that is local to the body of Csound instrument where it's defined.
-- The array contains control signals. It fills the array from the list of values (the last argument).
--
-- > fillLocalCtrlArr sizes initValues = ...
fillLocalCtrlArr :: (Tuple a, Tuple ix) => [Int] -> [a] -> SE (Arr ix a)
fillLocalCtrlArr = fillArrBy newLocalCtrlArrVar

-- | Creates a global array. The array contains control signals. It fills the array from the list of values (the last argument).
--
-- > fillGlobalCtrlArr sizes initValues = ...
fillGlobalCtrlArr :: (Tuple a, Tuple ix) => [Int] -> [a] -> SE (Arr ix a)
fillGlobalCtrlArr = fillArrBy newGlobalCtrlArrVar

newLocalCtrlArrVar, newGlobalCtrlArrVar :: Rate -> Run [E] -> Dep Var

newLocalCtrlArrVar  = D.newLocalArrVar  . toCtrlRate
newGlobalCtrlArrVar = newGlobalArrVar . toCtrlRate

toCtrlRate :: Rate -> Rate
toCtrlRate x = case x of
    Ar -> Kr
    Kr -> Ir
    _  -> x

-- | Reads data from the array.
readArr :: (Tuple a, Tuple ix) => Arr ix a -> ix -> SE a
readArr (Arr vars) ixs = fmap (toTuple . return) $ SE $ do
    ixsExp <- lift (fromTuple ixs)
    mapM (\v -> read' v ixsExp) vars
    where
        read' ::  Var -> [E] -> Dep E
        read' = D.readArr

-- | Writes data to the array.
writeArr :: (Tuple ix, Tuple a) => Arr ix a -> ix -> a -> SE ()
writeArr (Arr vars) ixs b = SE $ do
    ixsExp <- lift (fromTuple ixs)
    bsExp <- lift (fromTuple b)
    zipWithM_ (\var value -> write var ixsExp value) vars bsExp
    where
        write ::  Var -> [E] -> E -> Dep ()
        write = D.writeArr

-- | Writes data to the array.
writeInitArr :: (Tuple ix, Tuple a) => Arr ix a -> ix -> a -> SE ()
writeInitArr (Arr vars) ixs b = SE $ do
    ixsExp <- lift (fromTuple ixs)
    bsExp <- lift (fromTuple b)
    zipWithM_ (\var value -> write var ixsExp value) vars bsExp
    where
        write ::  Var -> [E] -> E -> Dep ()
        write = D.writeInitArr

-- | Updates the value of the array with pure function.
modifyArr :: (Tuple a, Tuple ix) => Arr ix a -> ix -> (a -> a) -> SE ()
modifyArr ref ixs f = do
    value <- readArr ref ixs
    writeArr ref ixs (f value)

mixArr :: (Tuple ix, Tuple a, Num a) => Arr ix a -> ix -> a -> SE ()
mixArr ref ixs a = modifyArr ref ixs (+ a)

-----------------------------------------------------
-- opcodes with array allocation

-- | Multiplies two numeric arrays and save the result in the third array.
mulArrayNew :: (Tuple b, Num b) => Arr a b -> Arr a b -> SE (Arr a b)
mulArrayNew = binOp "*"

-- | Summs two numeric arrays and save the result in the third array.
addArrayNew :: (Tuple b, Num b) => Arr a b -> Arr a b -> SE (Arr a b)
addArrayNew = binOp "+"

-- | Substraction of two numeric arrays and save the result in the third array.
subArrayNew :: (Tuple b, Num b) => Arr a b -> Arr a b -> SE (Arr a b)
subArrayNew = binOp "-"

-- | Division of two numeric arrays and save the result in the third array.
divArrayNew :: (Tuple b, Num b) => Arr a b -> Arr a b -> SE (Arr a b)
divArrayNew = binOp "/"

lenarray :: SigOrD c => Arr a b -> c
lenarray (Arr vs) = fromE $ return $ f (D.inlineVar $ head vs)
    where f a = D.opcs "lenarray" [(Kr, [Xr, Ir]), (Ir, [Xr, Ir])] [a]

-- | Copies table to array.
copyf2array :: Arr Sig Sig -> Tab -> SE ()
copyf2array (Arr vs) t = SE $ do
    tabExp <- lift (toE t)
    D.depT_ $ D.opcs "copyf2array" [(Xr, [D.varRate $ head vs, Ir])] [D.inlineVar $ head vs, tabExp]

-- | Copies array to table.
copya2ftab :: Arr Sig Sig -> Tab -> SE ()
copya2ftab (Arr vs) t = SE $ do
    tabExp <- lift (toE t)
    D.depT_ $ D.opcs "copya2ftab" [(Xr, [D.varRate $ head vs, Ir])] [D.inlineVar $ head vs, tabExp]

-- | Mapps all values in the array with the function.
--
-- Csound docs: <http://csound.github.io/docs/manual/maparray.html>
maparrayNew :: Arr a b -> Str -> SE (Arr a b)
maparrayNew (Arr vs) s = SE $ fmap Arr $ do
    strExp <- lift (toE s)
    mapM (\var -> go var strExp) vs
    where
        go var strExp = do
            outVar <- D.newTmpArrVar (D.varRate var)
            D.opcsArr isArrayInit outVar "slicearray" idRate [D.inlineVar var, strExp]
            return $ outVar

        idRate = fmap (\rate -> (rate, [rate, Ir, Ir])) [Ir, Kr, Ar]

-- | Finds a minimum value of the array.
minarray :: (Tuple b, Num b) => Arr a b -> SE b
minarray = extractArray "minarray"

-- | Finds a maximum value of the array.
maxarray :: (Tuple b, Num b) => Arr a b -> SE b
maxarray = extractArray "maxarray"

-- | Summs all elements in the array.
sumarray :: (Tuple b, Num b) => Arr a b -> SE b
sumarray = extractArray "sumarray"

-- | Scales all elements in the array.
scalearray :: (Tuple b, Num b) => Arr a b -> (b, b) -> SE ()
scalearray (Arr vs) (a, b) = SE $ do
    aExps <- lift (fromTuple a)
    bExps <- lift (fromTuple b)
    zipWithM_ (\var (aExp, bExp) -> go var (aExp, bExp)) vs (zip aExps bExps)
    where
        go v (aExp, bExp) =
            D.depT_ $ D.opcs "copyf2array" [(Xr, [D.varRate $ head vs, Ir])] [D.inlineVar v, aExp, bExp]

-- | Creates a copy of some part of the given array
slicearrayNew :: Arr D a -> (D, D) -> SE (Arr D a)
slicearrayNew (Arr vs) (from, to) = SE $ fmap Arr $ do
    fromExpr <- lift (toE from)
    toExpr   <- lift (toE to)
    mapM (\var -> go var (fromExpr, toExpr)) vs
    where
        go var (fromExpr, toExpr) = do
            outVar <- D.newTmpArrVar (D.varRate var)
            D.opcsArr isArrayInit outVar "slicearray" idRate [D.inlineVar var, fromExpr, toExpr]
            return $ outVar

        idRate = fmap (\rate -> (rate, [rate, Ir, Ir])) [Ir, Kr, Ar]

-- spectral opcodes

-- | Spectral array.
type SpecArr = Arr Sig Sig

-- |  Complex-to-complex Fast Fourier Transform.
--
-- csound docs: <http://csound.github.io/docs/manual/fft.html>
fftNew :: SpecArr -> SE SpecArr
fftNew = convert "fft"

-- | Complex-to-complex Inverse Fast Fourier Transform.
--
-- csound docs: <http://csound.github.io/docs/manual/fftinv.html>
fftinvNew :: SpecArr -> SE SpecArr
fftinvNew = convert "fftinvi"

-- | Fast Fourier Transform of a real-value array.
--
-- csound docs: <http://csound.github.io/docs/manual/rfft.html>
rfftNew :: SpecArr -> SE SpecArr
rfftNew = convert "rfft"

-- | Complex-to-real Inverse Fast Fourier Transform.
--
-- csound docs: <http://csound.github.io/docs/manual/rifft.html>
rifftNew :: SpecArr -> SE SpecArr
rifftNew = convert "rifft"

-- | Copies spectral data to k-rate arrays (or t-variables). Also known as pvs2array.
--
-- csound docs: <http://csound.github.io/docs/manual/pvs2tab.html>
pvs2tab :: SpecArr -> Spec -> SE Sig
pvs2tab = extractWith "pvs2tab" (Kr, [Xr, Fr])

-- | Copies spectral data from k-rate arrays (or t-variables.). Also known as pvsfromarray.
--
-- csound docs: <http://csound.github.io/docs/manual/tab2pvs.html>
tab2pvs :: SpecArr -> SE Spec
tab2pvs = extract1 Fr "tab2pvs"

-- | Complex product of two arrays.
--
-- > kout[] cmplxprod kin1[], kin2[]
--
-- csound docs: <http://csound.github.io/docs/manual/cmplxprod.html>
cmplxprodNew :: SpecArr -> SpecArr -> SE SpecArr
cmplxprodNew = convert2 "cmplxprod"

-- |  Rectangular to polar format conversion.
--
-- > kout[] rect2pol kin[]
--
-- csound docs: <http://csound.github.io/docs/manual/rect2pol.html>
rect2polNew :: SpecArr -> SE SpecArr
rect2polNew = convert "rect2pol"

-- | Polar to rectangular format conversion.
--
-- > kout[] pol2rect kin[]
--
-- csound docs: <http://csound.github.io/docs/manual/pol2rect.html>
pol2rectNew :: SpecArr -> SE SpecArr
pol2rectNew = convert "pol2rect"


-- | Polar to rectangular format conversion.
--
-- > kout[] pol2rect kmags[], kphs[]
--
-- csound docs: <http://csound.github.io/docs/manual/pol2rect.html>
pol2rect2New :: SpecArr -> SpecArr -> SE SpecArr
pol2rect2New = convert2 "pol2rect"

-- | Applies a window to an array.
--
-- > kout[] window kin[][, koff, itype]
--
-- csound docs: <http://csound.github.io/docs/manual/window.html>
windowArrayNew :: SpecArr -> SE SpecArr
windowArrayNew = convert "window"

-- | Real to complex format conversion.
--
-- > kout[] r2c kin[]
--
-- csound docs: <http://csound.github.io/docs/manual/r2c.html>
r2cNew :: SpecArr -> SE SpecArr
r2cNew = convert "r2c"

-- | Complex to real format conversion.
--
-- > kout[] c2r kin[]
--
-- csound docs: <http://csound.github.io/docs/manual/c2r.html>
c2rNew :: SpecArr -> SE SpecArr
c2rNew = convert "c2r"

-- | Obtains the magnitudes of a complex-number array
--
-- > kout[] mags kin[]
--
-- csound docs: <http://csound.github.io/docs/manual/mags.html>
magsArrayNew :: SpecArr -> SE SpecArr
magsArrayNew = convert "mags"

-- | Obtains the phases of a complex-number array
--
-- kout[] phs kin[]
--
-- > csound docs: <http://csound.github.io/docs/manual/phs.html>
phsArrayNew :: SpecArr -> SE SpecArr
phsArrayNew = convert "phs"

-----------------------------

isArrayInit, noArrayInit :: Bool

isArrayInit = True
noArrayInit = False

binOp :: Text -> Arr a b -> Arr a b -> SE (Arr a b)
binOp name (Arr xs) (Arr ys) = fmap Arr $ zipWithM go xs ys
    where
        go x y = SE $ do
            outVar <- D.newTmpArrVar (D.varRate x)
            D.infOprArr isArrayInit outVar name (D.inlineVar x) (D.inlineVar y)
            return outVar

convert :: Text -> Arr a b -> SE (Arr a b)
convert name (Arr vars) = fmap Arr $ mapM go vars
    where
        go v = SE $ do
            outVar <- D.newTmpArrVar (D.varRate v)
            D.opcsArr isArrayInit outVar name idRate1 [D.inlineVar v]
            return outVar

        idRate1 = fmap (\r -> (r, [r])) [Kr, Ar, Ir, Sr, Fr]

convert2 :: Text -> Arr a b -> Arr a b -> SE (Arr a b)
convert2 name (Arr xs) (Arr ys) = fmap Arr $ zipWithM go xs ys
    where
        go x y = SE $ do
            outVar <- D.newTmpArrVar (D.varRate x)
            D.opcsArr isArrayInit outVar name idRate2 [D.inlineVar x, D.inlineVar y]
            return outVar

        idRate2 = fmap (\r -> (r, [r, r])) [Kr, Ar, Ir, Sr, Fr]

extractArray :: (Tuple b) => Text -> Arr a b -> SE b
extractArray name (Arr vs) = SE $ fmap (toTuple . return) $ mapM (f . D.inlineVar) vs
    where f a = D.depT $ D.opcs name [(Xr, [Xr])] [a]

extract1 :: (Tuple b, Tuple c) => Rate -> Text -> Arr a b -> SE c
extract1 rate name (Arr vs) = SE $ fmap (toTuple . return) $ mapM (f . D.inlineVar) vs
    where f a = D.depT $ D.opcs name [(rate, [Xr])] [a]

extractWith :: (Tuple b, Tuple c, Tuple d) => Text -> (Rate, [Rate]) -> Arr a b -> c -> SE d
extractWith name rates (Arr vs) argument = SE $ fmap (toTuple . return) $ do
  argExps <- lift (fromTuple argument)
  zipWithM (\var x -> f (D.inlineVar var) x) vs argExps
  where f a b = D.depT $ D.opcs name [rates] [a, b]

---------------------------------------------------
-- opcodes with copy

-- | Transforms the dta of the array and copies it to the second array.
maparrayCopy :: Arr a b -> Str -> Arr a b -> SE ()
maparrayCopy (Arr vs) s (Arr outs) = SE $ do
    strExp <- lift (toE s)
    zipWithM_ (\var outVar -> go var strExp outVar) vs outs
    where
        go var strExp outVar = D.opcsArr noArrayInit outVar "slicearray" idRate [D.inlineVar var, strExp]
        idRate = fmap (\rate -> (rate, [rate, Ir, Ir])) [Ir, Kr, Ar]

-- | Copies a part of array to another array.
slicearrayCopy :: Arr D a -> (D, D) -> Arr D a -> SE ()
slicearrayCopy (Arr vs) (from, to) (Arr outs) = SE $ do
    fromExpr <- lift $ toE from
    toExpr   <- lift $ toE to
    zipWithM_ (\var outVar -> go var (fromExpr, toExpr) outVar) vs outs
    where
        go :: Var -> (E, E) -> Var -> Dep ()
        go var (fromExpr, toExpr) outVar = D.opcsArr noArrayInit outVar "slicearray" idRate [D.inlineVar var, fromExpr, toExpr]

        idRate = fmap (\rate -> (rate, [rate, Ir, Ir])) [Ir, Kr, Ar]

-- | Multiplies two arrays and copies the result into third array.
mulArrayCopy :: (Tuple b, Num b) => Arr a b -> Arr a b -> Arr a b -> SE ()
mulArrayCopy = binOpCopy "*"

-- | Summs two arrays and copies the result into third array.
addArrayCopy :: (Tuple b, Num b) => Arr a b -> Arr a b -> Arr a b -> SE ()
addArrayCopy = binOpCopy "+"

-- | Substracts two arrays and copies the result into third array.
subArrayCopy :: (Tuple b, Num b) => Arr a b -> Arr a b -> Arr a b -> SE ()
subArrayCopy = binOpCopy "-"

-- | Divides two arrays and copies the result into third array.
divArrayCopy :: (Tuple b, Num b) => Arr a b -> Arr a b -> Arr a b -> SE ()
divArrayCopy = binOpCopy "/"

-- spectral opcodes


-- |  Complex-to-complex Fast Fourier Transform.
--
-- csound docs: <http://csound.github.io/docs/manual/fft.html>
fftCopy :: SpecArr -> SpecArr -> SE ()
fftCopy = convertCopy "fft"

-- | Complex-to-complex Inverse Fast Fourier Transform.
--
-- csound docs: <http://csound.github.io/docs/manual/fftinv.html>
fftinvCopy :: SpecArr -> SpecArr -> SE ()
fftinvCopy = convertCopy "fftinvi"

-- | Fast Fourier Transform of a real-value array.
--
-- csound docs: <http://csound.github.io/docs/manual/rfft.html>
rfftCopy :: SpecArr -> SpecArr -> SE ()
rfftCopy = convertCopy "rfft"

-- | Complex-to-real Inverse Fast Fourier Transform.
--
-- csound docs: <http://csound.github.io/docs/manual/rifft.html>
rifftCopy :: SpecArr -> SpecArr -> SE ()
rifftCopy = convertCopy "rifft"


-- | Complex product of two arrays.
--
-- > kout[] cmplxprod kin1[], kin2[]
--
-- csound docs: <http://csound.github.io/docs/manual/cmplxprod.html>
cmplxprodCopy :: SpecArr -> SpecArr -> SpecArr -> SE ()
cmplxprodCopy = convert2Copy "cmplxprod"

-- |  Rectangular to polar format conversion.
--
-- > kout[] rect2pol kin[]
--
-- csound docs: <http://csound.github.io/docs/manual/rect2pol.html>
rect2polCopy :: SpecArr -> SpecArr -> SE ()
rect2polCopy = convertCopy "rect2pol"

-- | Polar to rectangular format conversion.
--
-- > kout[] pol2rect kin[]
--
-- csound docs: <http://csound.github.io/docs/manual/pol2rect.html>
pol2rectCopy :: SpecArr -> SpecArr -> SE ()
pol2rectCopy = convertCopy "pol2rect"

-- | Polar to rectangular format conversion.
--
-- > kout[] pol2rect kmags[], kphs[]
--
-- csound docs: <http://csound.github.io/docs/manual/pol2rect.html>
pol2rect2Copy :: SpecArr -> SpecArr -> SpecArr -> SE ()
pol2rect2Copy = convert2Copy "pol2rect2"

-- | Applies a window to an array.
--
-- > kout[] window kin[][, koff, itype]
--
-- csound docs: <http://csound.github.io/docs/manual/window.html>
windowArrayCopy :: SpecArr -> SpecArr -> SE ()
windowArrayCopy = convertCopy "window"

-- | Real to complex format conversion.
--
-- > kout[] r2c kin[]
--
-- csound docs: <http://csound.github.io/docs/manual/r2c.html>
r2cCopy :: SpecArr -> SpecArr -> SE ()
r2cCopy = convertCopy "r2c"

-- | Complex to real format conversion.
--
-- > kout[] c2r kin[]
--
-- csound docs: <http://csound.github.io/docs/manual/c2r.html>
c2rCopy :: SpecArr -> SpecArr -> SE ()
c2rCopy = convertCopy "c2r"


-- | Obtains the magnitudes of a complex-number array
--
-- > kout[] mags kin[]
--
-- csound docs: <http://csound.github.io/docs/manual/mags.html>
magsArrayCopy :: SpecArr -> SpecArr -> SE ()
magsArrayCopy = convertCopy "mags"

-- | Obtains the phases of a complex-number array
--
-- kout[] phs kin[]
--
-- > csound docs: <http://csound.github.io/docs/manual/phs.html>
phsArrayCopy :: SpecArr -> SpecArr -> SE ()
phsArrayCopy = convertCopy "phs"

---------------------------------------------------------------

binOpCopy :: Text -> Arr a b -> Arr a b -> Arr a b -> SE ()
binOpCopy name (Arr xs) (Arr ys) (Arr outs) = mapM_ go $ zip3 xs ys outs
  where
    go (x, y, outVar) = SE $ D.infOprArr noArrayInit outVar name (D.inlineVar x) (D.inlineVar y)

convertCopy :: Text -> Arr a b -> Arr a b -> SE ()
convertCopy name (Arr vars) (Arr outs) = zipWithM_ go vars outs
  where
    go v outVar = SE $ D.opcsArr noArrayInit outVar name idRate1 [D.inlineVar v]
    idRate1 = fmap (\r -> (r, [r])) [Kr, Ar, Ir, Sr, Fr]

convert2Copy :: Text -> Arr a b -> Arr a b -> Arr a b -> SE ()
convert2Copy name (Arr xs) (Arr ys) (Arr outs) = mapM_ go $ zip3 xs ys outs
  where
    go (x, y, outVar) = SE $ D.opcsArr noArrayInit outVar name idRate2 [D.inlineVar x, D.inlineVar y]
    idRate2 = fmap (\r -> (r, [r, r])) [Kr, Ar, Ir, Sr, Fr]
