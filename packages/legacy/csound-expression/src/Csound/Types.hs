{- | The Csound types.

There are several primitive types:

* @Sig@ - signals

* @D@ - numbers

* @Str@ - strings

* @Tab@ - 1-dimensional arrays

* @Spec@ and @Wspec@ - sound spectrums

 A signal is a stream of numbers. Signals carry sound or time varied
 control values. Numbers are constants. 1-dimensional arrays contain some useful
 data which is calculated at the initial run of the program.

There is only one compound type. It's a tuple of Csound values. The empty tuple
is signified with special type called @Unit@.
-}
module Csound.Types (
  -- * Primitive types
  Sig,
  D,
  Tab,
  Str,
  Spec,
  Wspec,
  BoolSig,
  BoolD,
  Val (..),
  SigOrD,
  Sig2,
  Sig3,
  Sig4,
  Sig5,
  Sig6,
  Sig8,
  Sig2_2,
  Sig2_3,
  Sig2_4,
  Sig2_5,
  Sig2_6,
  Sig2_7,
  Sig2_8,

  -- ** Constructors
  double,
  int,
  text,

  -- ** Constants
  idur,
  getSampleRate,
  getControlRate,
  getBlockSize,

  -- ** BPM
  getBpm,
  setBpm,
  syn,
  takt,

  -- ** Converters
  ar,
  kr,
  ir,
  sig,

  -- ** Init values
  withInits,
  withDs,
  withSigs,
  withTabs,
  withD,
  withSig,
  withTab,
  withSeed,

  -- ** Numeric functions
  quot',
  rem',
  div',
  mod',
  ceil',
  floor',
  round',
  int',
  frac',

  -- ** Logic functions
  boolSig,
  when1,
  whens,
  whenElse,
  whenD1,
  whenDs,
  whileDo,
  untilDo,
  whileDoD,
  untilDoD,
  whenElseD,
  compareWhenD,
  equalsTo,
  notEqualsTo,
  lessThan,
  greaterThan,
  lessThanEquals,
  greaterThanEquals,

  -- ** Aliases

  -- | Handy for functions that return tuples to specify the utput type
  --
  -- > (aleft, aright) = ar2 $ diskin2 "file.wav" 1
  --
  -- or
  --
  -- > asig = ar1 $ diskin2 "file.wav" 1
  ar1,
  ar2,
  ar4,
  ar6,
  ar8,

  -- * Tuples
  Tuple (..),
  makeTupleMethods,
  Unit,
  unit,
  atTuple,

  -- *** Logic functions
  ifTuple,
  guardedTuple,
  caseTuple,

  -- * Instruments

  -- | An instrument is a function that takes a tpule of csound values as an argument
  -- and returns a tuple of signals as an output. The type of the instrument is:
  --
  -- > (Arg a, Out b) => a -> b

  -- ** Arguments
  Arg,
  atArg,

  -- *** Logic functions
  ifArg,
  guardedArg,
  caseArg,

  -- ** Monophonic arguments
  MonoArg (..),
  MonoAdsr,
  adsrMonoSynt,
  monoAdsr,

  -- ** Outputs
  Sigs,

  -- * Arrays
  Arr,
  newLocalArr,
  newGlobalArr,
  newLocalCtrlArr,
  newGlobalCtrlArr,
  writeArr,
  readArr,
  modifyArr,
  mixArr,

  -- ** Type inference helpers
  Arr1,
  DArr1,
  Arr2,
  DArr2,
  Arr3,
  DArr3,
  arr1,
  darr1,
  arr2,
  darr2,
  arr3,
  darr3,

  -- ** Traverse and fold
  foreachArr,
  foreachArrD,
  forRowArr,
  forColumnArr,
  forRowArrD,
  forColumnArrD,
  foldArr,
  foldRowArr,
  foldColumnArr,
  foldRowsArrD,
  foldColumnsArrD,

  -- ** Array opcodes
  fillLocalArr,
  fillGlobalArr,
  fillLocalCtrlArr,
  fillGlobalCtrlArr,
  maparrayNew,
  lenarray,
  copyf2array,
  copya2ftab,
  minarray,
  maxarray,
  sumarray,
  scalearray,
  slicearrayNew,
  maparrayCopy,
  slicearrayCopy,

  -- * Pure Arrays (read-only)
  PureArr,
  newPureArr,
  readPureArr,
  PureArrD,
  newPureArrD,
  readPureArrD,

  -- ** Spectral opcodes
  SpecArr,
  fftNew,
  fftinvNew,
  rfftNew,
  rifftNew,
  pvs2tab,
  tab2pvs,
  cmplxprodNew,
  rect2polNew,
  pol2rectNew,
  pol2rect2New,
  windowArrayNew,
  r2cNew,
  c2rNew,
  magsArrayNew,
  phsArrayNew,
  fftCopy,
  fftinvCopy,
  rfftCopy,
  rifftCopy,
  cmplxprodCopy,
  rect2polCopy,
  pol2rectCopy,
  pol2rect2Copy,
  windowArrayCopy,
  r2cCopy,
  c2rCopy,
  magsArrayCopy,
  phsArrayCopy,
) where

import Csound.Typed.Control
import Csound.Typed.Types
import Data.Boolean

-- | Gets an init-rate value from the list by index.
atArg :: (Tuple a) => [a] -> Sig -> a
atArg as ind = readPureArr arr ind
  where
    arr = newPureArr as

-- guardedArg (zip (fmap (\x -> sig (int x) ==* ind) [0 .. ]) as) (head as)

-- | Gets an control/audio-rate value from the list by index.
atTuple :: (Tuple a) => [a] -> Sig -> a
atTuple as ind = readPureArr arr ind
  where
    arr = newPureArr as

-- guardedTuple (zip (fmap (\x -> sig (int x) ==* ind) [0 .. ]) as) (head as)

whenElseD :: BoolD -> SE () -> SE () -> SE ()
whenElseD condition ifDo elseDo = whenDs [(condition, ifDo)] elseDo

whenElse :: BoolSig -> SE () -> SE () -> SE ()
whenElse condition ifDo elseDo = whens [(condition, ifDo)] elseDo

-- | Performs tree search f the first argument lies within the interval it performs the corresponding procedure.
compareWhenD :: D -> [(D, SE ())] -> SE ()
compareWhenD val conditions = case conditions of
  [] -> return ()
  [(_condition, ifDo)] -> ifDo
  (condition1, do1) : (_condition2, do2) : [] -> whenElseD (val `lessThan` condition1) do1 do2
  _ -> whenElseD (val `lessThan` rootcondition) (compareWhenD val less) (compareWhenD val more)
  where
    (less, more) = splitAt (length conditions `div` 2) conditions
    rootcondition = fst $ last less

{- | It's used to synchronize changes with BPM.
Useful with LFO's or delay times.
-}
syn :: Sig -> Sig
syn x = (bpm / 60) * x
  where
    bpm = getBpm

{- | Calculates seconditions in ratio to global BPM.
It measures time according to changes in the BPM.
It's reciprocal to @syn@.
-}
takt :: Sig -> Sig
takt = recip . syn
