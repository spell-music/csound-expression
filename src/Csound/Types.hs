-- | The Csound types.
--
-- There are several primitive types:
--
-- * @Sig@ - signals
--
-- * @D@ - numbers
--
-- * @Str@ - strings
--
-- * @Tab@ - 1-dimensional arrays
--
-- * @Spec@ and @Wspec@ - sound spectrums
--
--  A signal is a stream of numbers. Signals carry sound or time varied 
--  control values. Numbers are constants. 1-dimensional arrays contain some useful
--  data which is calculated at the initial run of the program.
--
-- There is only one compound type. It's a tuple of Csound values. The empty tuple
-- is signified with special type called @Unit@. 
--
module Csound.Types(
    -- * Primitive types    
    Sig, D, Tab, Str, Spec, Wspec,    
    BoolSig, BoolD, Val(..), SigOrD,    

    Sig2, Sig3, Sig4, Sig5, Sig6, Sig8,
    -- ** Constructors
    double, int, text, 
    
    -- ** Constants
    idur, getSampleRate, getControlRate, getBlockSize,

    -- ** Converters
    ar, kr, ir, sig,

    -- ** Init values
    withInits, withDs, withSigs, withTabs, 
    withD, withSig, withTab, withSeed,

    -- ** Numeric functions
    quot', rem', div', mod', ceil', floor', round', int', frac',        
   
    -- ** Logic functions
    boolSig, when1, whens, whenElse, whenD1, whenDs, whileDo, untilDo, whileDoD, untilDoD, whenElseD, compareWhenD,
    equalsTo, notEqualsTo, lessThan, greaterThan, lessThanEquals, greaterThanEquals,

    -- ** Aliases 
    -- | Handy for functions that return tuples to specify the utput type
    --
    -- > (aleft, aright) = ar2 $ diskin2 "file.wav" 1
    -- 
    -- or
    --
    -- > asig = ar1 $ diskin2 "file.wav" 1    
    ar1, ar2, ar4, ar6, ar8,

    -- * Tuples
    Tuple(..), makeTupleMethods, Unit, unit, atTuple,
    -- *** Logic functions
    ifTuple, guardedTuple, caseTuple, 
    
    -- * Instruments    

    -- | An instrument is a function that takes a tpule of csound values as an argument
    -- and returns a tuple of signals as an output. The type of the instrument is:
    --
    -- > (Arg a, Out b) => a -> b

    -- ** Arguments
    Arg, atArg,
    -- *** Logic functions
    ifArg, guardedArg, caseArg, 

    -- ** Monophonic arguments
    MonoArg(..), MonoAdsr, adsrMonoSynt, monoAdsr,

    -- ** Outputs
    Sigs,

    -- * Arrays
    Arr, newLocalArr, newGlobalArr, newLocalCtrlArr, newGlobalCtrlArr, 
    writeArr, readArr, modifyArr, mixArr,
    -- ** Type inference helpers
    Arr1, DArr1, Arr2, DArr2, Arr3, DArr3,
    arr1, darr1, arr2, darr2, arr3, darr3,  

    -- ** Array opcodes
    fillLocalArrayNew, fillGlobalArrayNew, fillLocalCtrlArrayNew, fillGlobalCtrlArrayNew,
    maparrayNew, lenarray, copyf2array, copya2ftab, minarray, maxarray, sumarray, 
    scalearray, slicearrayNew,

    fillArrayCopy, maparrayCopy, slicearrayCopy,

    -- ** Spectral opcodes
    SpecArr, 

    fftNew, fftinvNew, rfftNew, rifftNew, pvs2tab, tab2pvs, cmplxprodNew, 
    rect2polNew, pol2rectNew, pol2rect2New, windowArrayNew, 
    r2cNew, c2rNew, magsArrayNew, phsArrayNew,

    fftCopy, fftinvCopy, rfftCopy, rifftCopy, cmplxprodCopy, 
    rect2polCopy, pol2rectCopy, pol2rect2Copy, windowArrayCopy, 
    r2cCopy, c2rCopy, magsArrayCopy, phsArrayCopy
) where

import Data.Boolean
import Csound.Typed.Types
import Csound.Control.SE

-- | Gets an init-rate value from the list by index.
atArg :: (Tuple a, Arg a) => [a] -> D -> a
atArg as ind = guardedArg (zip (fmap (\x -> int x ==* ind) [0 .. ]) as) (head as)

-- | Gets an control/audio-rate value from the list by index.
atTuple :: (Tuple a) => [a] -> Sig -> a
atTuple as ind = guardedTuple (zip (fmap (\x -> sig (int x) ==* ind) [0 .. ]) as) (head as)


whenElseD :: BoolD -> SE () -> SE () -> SE ()
whenElseD cond ifDo elseDo = whenDs [(cond, ifDo)] elseDo

whenElse :: BoolSig -> SE () -> SE () -> SE () 
whenElse cond ifDo elseDo = whens [(cond, ifDo)] elseDo

-- | Performs tree search f the first argument lies within the interval it performs the corresponding procedure.
compareWhenD :: D -> [(D, SE ())] -> SE ()
compareWhenD val conds = case conds of
    [] -> return ()
    [(cond, ifDo)] -> ifDo 
    (cond1, do1):(cond2, do2): [] -> whenElseD (val `lessThan` cond1) do1 do2
    _ -> whenElseD (val `lessThan` rootCond) (compareWhenD val less) (compareWhenD val more)
    where
        (less, more) = splitAt (length conds `div` 2) conds
        rootCond = fst $ last less
