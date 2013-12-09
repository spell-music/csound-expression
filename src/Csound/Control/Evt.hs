{-#Language BangPatterns, TupleSections, FlexibleContexts #-}
module Csound.Control.Evt(
    Evt(..), Bam, 

    -- * Core functions
    boolToEvt, evtToBool, sigToEvt, stepper,
    filterE, filterSE, accumSE, accumE, filterAccumE, filterAccumSE,
    Snap, snapshot, snaps, sync, syncBpm,
    
    -- * Opcodes
    metroE, changedE, triggerE, 

    -- * Higher-level event functions
    cycleE, iterateE, repeatE, appendE, mappendE,
    oneOf, freqOf, freqAccum, 
    randDs, randInts, randSkip, randSkipBy, 
    range, listAt,   
    every, masked        
) where

import Data.Monoid
import Data.Default
import Data.Boolean

import Csound.Typed
import Csound.Typed.Opcode

-- | Behaves like 'Csound.Opcode.Basic.metro', but returns an event stream.
metroE :: Sig -> Evt Unit 
metroE = sigToEvt . metro

-- | Behaves like 'Csound.Opcode.Basic.changed', but returns an event stream.
changedE :: [Sig] ->  Evt Unit
changedE = sigToEvt . changed

-- | Behaves like 'Csound.Opcode.Basic.trigger', but returns an event stream.
triggerE :: Sig -> Sig -> Sig -> Evt Unit 
triggerE a1 a2 a3 = sigToEvt $ trigger a1 a2 a3

-- | the sync function but time is measured in beats per minute.
syncBpm :: (Default a, Tuple a) => D -> Evt a -> Evt a
syncBpm dt = sync (dt / 60)

----------------------------------------------------------------------
-- higher level evt-funs

-- | Constructs an event stream that contains an infinite repetition
-- values from the given list. When an event happens this function takes 
-- the next value from the list, if there is no values left it starts
-- from the beggining of the list.
cycleE :: (Tuple a, Arg a) => [a] -> Evt b -> Evt a
cycleE vals evts = listAt vals $ range (0, int $ length vals) evts

-- | Turns an event of indices to the event of the values from the list.
-- A value is taken with index. 
listAt :: (Tuple a, Arg a) => [a] -> Evt D -> Evt a
listAt vals evt
    | null vals = mempty
    | otherwise = fmap (atArg vals) $ filterE within evt
    where
        within x = (x >=* 0) &&* (x <* len)
        len = int $ length vals

atArg :: (Tuple a, Arg a) => [a] -> D -> a
atArg as ind = guardedArg (zip (fmap (\x -> int x ==* ind) [0 .. ]) as) (head as)

-- | 
--
-- > range (xMin, xMax) === cycleE [xMin .. pred xMax]
range :: (D, D) -> Evt b -> Evt D
range (xMin, xMax) = iterateE xMin $ \x -> ifB ((x + 1) >=* xMax) xMin (x + 1)

-- | An event stream of the integers taken from the given diapason.
randInts :: (D, D) -> Evt b -> Evt D
randInts (xMin, xMax) = accumSE (0 :: D) $ const $ \s -> fmap (, s) $ getRnd
    where getRnd = fmap (int' . readSnap) $ random (sig $ int' xMin) (sig $ int' xMax)

-- | An event stream of the random values in the interval @(0, 1)@.
randDs :: Evt b -> Evt D
randDs = accumSE (0 :: D) $ const $ \s -> fmap (, s) $ fmap readSnap $ random (0::D) 1 

-- | Skips elements at random.
--
-- > randSkip prob
--
-- where @prob@ is probability of includinng the element in the output stream. 
randSkip :: D -> Evt a -> Evt a
randSkip d = filterSE (const $ fmap (<=* d) $ random (0::D) 1)

-- | Skips elements at random.
--
-- > randSkip probFun
--
-- It behaves just like @randSkip@, but probability depends on the value.
randSkipBy :: (a -> D) -> Evt a -> Evt a
randSkipBy d = filterSE (\x -> fmap (<=* d x) $ random (0::D) 1)

-- | When something happens on the given event stream resulting
-- event stream contains an application of some unary function to the 
-- given initial value. So the event stream contains the values:
--
-- > [s0, f s0, f (f s0), f (f (f s0)), ...]
iterateE :: (Tuple a) => a -> (a -> a) -> Evt b -> Evt a
iterateE s0 f = accumE s0 (const phi)
    where phi s = (s, f s)

-- | Substitutes all values in the input stream with the given constant value.
repeatE :: Tuple a => a -> Evt b -> Evt a
repeatE a = fmap (const a)

-- | Accumulates a values from the given event stream with binary function.
-- It's a variant of the fold for event streams.
--
-- > appendE z f evt
--
-- When value @a@ happens with @evt@, the resulting event stream contains
-- a value (z `f` a) and in the next time @z@ equals to this value.
appendE :: Tuple a => a -> (a -> a -> a) -> Evt a -> Evt a
appendE empty append = accumE empty phi
    where phi a s = let s1 = s `append` a in (s1, s1)

-- | A special variant of the function `appendE` for the monoids. 
-- Initial value is `mempty` and binary function is `mappend` which
-- belong to the instance of the class `Monoid`.
mappendE :: (Monoid a, Tuple a) => Evt a -> Evt a
mappendE = appendE mempty mappend

-- | Constructs an event stream that contains values from the
-- given list which are taken in the random order.
oneOf :: (Tuple a, Arg a) => [a] -> Evt b -> Evt a
oneOf vals evt = listAt vals $ randInts (0, int $ length vals) evt

-- | Represents a values with frequency of occurence.
type Rnds a = [(D, a)]


-- | Constructs an event stream that contains values from the
-- given list which are taken in the random order. In the list we specify
-- not only values but the frequencies of occurrence. Sum of the frequencies 
-- should be equal to one.
freqOf :: (Tuple a, Arg a) => Rnds a -> Evt b -> Evt a
freqOf rnds evt = fmap (takeByWeight accs vals) $ randDs evt 
    where
        accs = accumWeightList $ fmap fst rnds
        vals = fmap snd rnds

takeByWeight :: (Tuple a, Arg a) => [D] -> [a] -> D -> a
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
freqAccum :: (Tuple s, Tuple (b, s), Arg (b, s)) 
    => s -> (a -> s -> Rnds (b, s)) -> Evt a -> Evt b 
freqAccum s0 f = accumSE s0 $ \a s -> 
    let rnds = f a s
        accs = accumWeightList $ fmap fst rnds
        vals = fmap snd rnds
    in  fmap (takeByWeight accs vals . readSnap) $ random (0 :: D) 1

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
every :: (Tuple a, Arg a) => Int -> [Int] -> Evt a -> Evt a
every empties beats = masked mask  
    where mask = (fmap (\x -> if x then 1 else 0) $ (replicate empties False) ++ patternToMask beats)

-- | Filters events with the mask. A mask is a list of ones and zeroes. 
-- n'th element from the given list should be included in the resulting stream
-- if the n'th element from the list equals to one or skipped if the element 
-- equals to zero.
masked :: (Tuple a, Arg a) => [D] -> Evt a -> Evt a
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

