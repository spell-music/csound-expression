{-# Language TupleSections, BangPatterns, FlexibleContexts #-}
module Csound.Control.Evt(
    Evt(..), Snap, Bam,
    -- * Core functions
    boolToEvt, evtToBool, sigToEvt, filterE, 
    accumE, accumSE, filterAccumSE, filterAccumE, snapshot, snaps,
    -- * Auxiliary functions
    cycleE, iterateE, repeatE, appendE, mappendE,
    oneOf, freqOf, freqAccum, randDs, randInts, range, listAt,   
    every, masked        
) where

import Data.Monoid
import Data.Boolean

import Csound.Exp.Wrapper
import Csound.Exp.Event
import Csound.Exp.Tuple
import Csound.Exp.Arg
import Csound.Exp.Logic
import Csound.Exp.Numeric(intD)

import Csound.Render.Channel(random)

----------------------------------------------------------------------
-- higher level evt-funs

-- | Constructs an event stream that contains an infinite repetition
-- values from the given list. When an event happens this function takes 
-- the next value from the list, if there is no values left it starts
-- from the beggining of the list.
cycleE :: (CsdTuple a, Arg a) => [a] -> Evt b -> Evt a
cycleE vals evts = listAt vals $ range (0, int $ length vals) evts

-- | Turns an event of indices to the event of the values from the list.
-- A value is taken with index. 
listAt :: (CsdTuple a, Arg a) => [a] -> Evt D -> Evt a
listAt vals evt
    | null vals = mempty
    | otherwise = fmap (atArg vals) $ filterE within evt
    where
        within x = (x >=* 0) &&* (x <* len)
        len = int $ length vals

atArg :: (CsdTuple a, Arg a) => [a] -> D -> a
atArg as ind = guardedArg (zip (fmap (\x -> int x ==* ind) [0 .. ]) as) (head as)

-- | 
--
-- > range (xMin, xMax) === cycleE [xMin .. pred xMax]
range :: (D, D) -> Evt b -> Evt D
range (xMin, xMax) = iterateE xMin $ \x -> ifB (succ x >=* xMax) xMin (succ x)

-- | An event stream of the integers taken from the given diapason.
randInts :: (D, D) -> Evt b -> Evt D
randInts (xMin, xMax) = accumSE (0 :: D) $ const $ \s -> fmap (, s) $ rnd
    where rnd = fmap (intD . readSnap) $ random (sig $ intD xMin) (sig $ intD xMax)

-- | An event stream of the random values in the interval @(0, 1)@.
randDs :: Evt b -> Evt D
randDs = accumSE (0 :: D) $ const $ \s -> fmap (, s) $ fmap readSnap $ random 0 1 

-- | When something happens on the given event stream resulting
-- event stream contains an application of some unary function to the 
-- given initial value. So the event stream contains the values:
--
-- > [s0, f s0, f (f s0), f (f (f s0)), ...]
iterateE :: (CsdTuple a) => a -> (a -> a) -> Evt b -> Evt a
iterateE s0 f = accumE s0 (const phi)
    where phi s = (s, f s)

-- | Substitutes all values in the input stream with the given constant value.
repeatE :: CsdTuple a => a -> Evt b -> Evt a
repeatE a = fmap (const a)

-- | Accumulates a values from the given event stream with binary function.
-- It's a variant of the fold for event streams.
--
-- > appendE z f evt
--
-- When value @a@ happens with @evt@, the resulting event stream contains
-- a value (z `f` a) and in the next time @z@ equals to this value.
appendE :: CsdTuple a => a -> (a -> a -> a) => Evt a -> Evt a
appendE empty append = accumE empty phi
    where phi a s = let s1 = s `append` a in (s1, s1)

-- | A special variant of the function `appendE` for the monoids. 
-- Initial value is `mempty` and binary function is `mappend` which
-- belong to the instance of the class `Monoid`.
mappendE :: (Monoid a, CsdTuple a) => Evt a -> Evt a
mappendE = appendE mempty mappend

-- | Constructs an event stream that contains values from the
-- given list which are taken in the random order.
oneOf :: (CsdTuple a, Arg a) => [a] -> Evt b -> Evt a
oneOf vals evt = listAt vals $ randInts (0, int $ length vals) evt

-- | Represents a values with frequency of occurence.
type Rnds a = [(D, a)]


-- | Constructs an event stream that contains values from the
-- given list which are taken in the random order. In the list we specify
-- not only values but the frequencies of occurrence. Sum of the frequencies 
-- should be equal to one.
freqOf :: (CsdTuple a, Arg a) => Rnds a -> Evt b -> Evt a
freqOf rnds evt = fmap (takeByWeight accs vals) $ randDs evt 
    where
        accs = accumWeightList $ fmap fst rnds
        vals = fmap snd rnds

takeByWeight :: (CsdTuple a, Arg a) => [D] -> [a] -> D -> a
takeByWeight accumWeights vals at = 
    guardedArg (zipWith (\w val -> (at <* w, val)) accumWeights vals) (last vals)

accumWeightList :: Num a => [a] -> [a]
accumWeightList = go 0
    where go !s xs = case xs of
            []   -> []
            a:as -> a + s : go (a + s) as
   
-- | This function combines the functions 'Csound.Control.Evt.accumE' and
-- 'Csound.Control.Evt.freqOf'. We transform the values of the event stream
-- with stateful function that produce not just values but the list of values
-- with frequencies of occurrence. We apply this function to the current state
-- and the value and then at random pick one of the values.
freqAccum :: (CsdTuple s, CsdTuple (b, s), Arg (b, s)) 
    => s -> (a -> s -> Rnds (b, s)) -> Evt a -> Evt b 
freqAccum s0 f = accumSE s0 $ \a s -> 
    let rnds = f a s
        accs = accumWeightList $ fmap fst rnds
        vals = fmap snd rnds
    in  fmap (takeByWeight accs vals . readSnap) $ random 0 1

-- | Specialization of the function 'Csound.Control.Evt.masked'. 
--
-- > every n [a, b, c, ..] evt
--
-- constructs a mask that skips first @n@ elements and then produces
-- an event and skips next (a - 1) events, then produces an event and
-- skips next (b - 1) events and so on. It's useful for construction of 
-- the percussive beats. For example 
--
-- > every 0 [2] (metroE 2)
--
-- triggers an event on the odd beats. With this function we can 
-- create a complex patterns of cyclic events.
--
every :: (CsdTuple a, Arg a) => Int -> [Int] -> Evt a -> Evt a
every empties beats = masked mask  
    where mask = (fmap (\x -> if x then 1 else 0) $ (replicate empties False) ++ patternToMask beats)

-- | Filters events with the mask. A mask is a list of ones and zeroes. 
-- n'th element from the given list should be included in the resulting stream
-- if the n'th element from the list equals to one or skipped if the element 
-- equals to zero.
masked :: (CsdTuple a, Arg a) => [D] -> Evt a -> Evt a
masked ms = filterAccumE 0 $ \a s -> 
    let n  = int $ length ms
        s1 = ifB (s + 1 <* n) (s + 1) 0
    in  (atArg ms s ==* 1, a, s1)

patternToMask :: [Int] -> [Bool]
patternToMask xs = case xs of
    []   -> []
    a:as -> single a ++ patternToMask as
    where single n 
            | n <= 0    = []
            | otherwise = True : replicate (n - 1) False

