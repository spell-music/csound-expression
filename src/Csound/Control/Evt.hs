{-#Language BangPatterns, TupleSections, FlexibleContexts #-}
module Csound.Control.Evt(
    Evt(..), Bam, Tick, 

    -- * Core functions
    boolToEvt, evtToBool, sigToEvt, evtToSig, stepper,
    filterE, filterSE, accumSE, accumE, filterAccumE, filterAccumSE,

    Snap, snapshot, snaps, snaps2, sync, syncBpm, 
    
    -- * Opcodes
    metro, gaussTrig, dust, metroSig, dustSig, dustSig2, impulseE, changedE, triggerE, loadbang, impulse, metroE,

    -- * Higher-level event functions
    devt, eventList,
    cycleE, iterateE, repeatE, appendE, mappendE, partitionE, 
    takeE, dropE, takeWhileE, dropWhileE,
    splitToggle, toTog, toTog1,
    Rnds,
    oneOf, freqOf, freqAccum, 
    randDs, randList, randInts, randSkip, randSkipBy, 
    range, listAt,   
    every, masked        
) where

import Prelude hiding ((<*))

import Data.Monoid
import Data.Default
import Data.Boolean
import Data.Tuple

import Temporal.Media

import Csound.Typed hiding (evtToBool)
import Csound.Typed.Opcode hiding (metro, dust, dust2)
import qualified Csound.Typed.Opcode as O(metro, dust, dust2)
import Csound.Types(atArg)

type Tick = Evt Unit

evtToSig :: D -> (Evt D) -> Sig
evtToSig initVal evts = retrigs (return . sig) $ fmap return $ devt initVal loadbang <> evts

evtToBool :: Evt a -> BoolSig
evtToBool a = ( ==* 1) $ changed $ return $ evtToSig 0 $ cycleE [1, 0] a

-- | Constant event stream. It produces the same value (the first argument)
-- all the time.
devt :: D -> Evt a -> Evt D
devt d = fmap (const d)

{-# DEPRECATED metroE "Use metro instead" #-}
-- | Behaves like 'Csound.Opcode.Basic.metro', but returns an event stream.
metroE :: Sig -> Evt Unit 
metroE = sigToEvt . O.metro

-- | Creates a stream of events that happen with the given frequency.
metro :: Sig -> Evt Unit 
metro = sigToEvt . O.metro

-- | Csound's original metro function.
metroSig :: Sig -> Sig
metroSig = O.metro

-- | Creates a stream of ticks that happen around the given frequency with given deviation.
-- 
-- > gaussTrig freq deviation
gaussTrig :: Sig -> Sig -> Tick
gaussTrig afreq adev = Evt $ \bam -> do
    on <- gausstrig 1 (afreq * sig getBlockSize) adev
    when1 (on >* 0.5) $ bam unit

-- | Creates a stream of random events. The argument is a number of events per second.
--
-- > dust eventsPerSecond
dust :: Sig -> Tick
dust freq = Evt $ \bam -> do
    on <- O.dust 1 (freq * sig getBlockSize)
    when1 (on >* 0.5) $ bam unit

-- | Creates a signal that contains a random ones that happen with given frequency.
dustSig :: Sig -> SE Sig
dustSig freq = O.dust 1 (freq * sig getBlockSize)

-- | Creates a signal that contains a random ones or negative ones that happen with given frequency.
dustSig2 :: Sig -> SE Sig
dustSig2 freq = O.dust2 1 (freq * sig getBlockSize)

-- | Fires a single event right now.
--
-- > loadbang = pulseE 0
loadbang :: Evt Unit
loadbang = impulseE 0

-- | Fires a single true value in the given time ahead.
impulse :: D -> Sig 
impulse dt = downsamp (mpulse (sig $ getBlockSize) 0 `withD` dt) `withD` getBlockSize

-- | Fires a single event in the given time ahead.
impulseE :: D -> Evt Unit
impulseE = sigToEvt . impulse

-- | Makes an event stream from list of events.
eventList :: [(D, D, a)] -> Evt (Sco a)
eventList es = fmap (const $ har $ fmap singleEvent es) loadbang
    where singleEvent (start, duration, content) = del start $ str duration $ temp content

-- | Behaves like 'Csound.Opcode.Basic.changed', but returns an event stream.
changedE :: [Sig] ->  Evt Unit
changedE = sigToEvt . changed

-- | Behaves like 'Csound.Opcode.Basic.trigger', but returns an event stream.
triggerE :: Sig -> Sig -> Sig -> Evt Unit 
triggerE a1 a2 a3 = sigToEvt $ trigger a1 a2 a3

-- | the sync function but time is measured in beats per minute.
syncBpm :: (Default a, Tuple a) => Sig -> Evt a -> Evt a
syncBpm dt = sync (dt / 60)

-- | Splits event stream on two streams with predicate.
partitionE :: (a -> BoolD) -> Evt a -> (Evt a, Evt a)
partitionE p evts = (a, b)
    where 
        a = filterE p          evts
        b = filterE (notB . p) evts

-- | Splits a toggle event stream on on-events and off-events.
splitToggle :: Evt D -> (Evt D, Evt D)
splitToggle = swap . partitionE (==* 0)

-- | Constructs an event stream that contains pairs from the
-- given pair of signals. Events happens when any signal changes.
snaps2 :: Sig2 -> Evt (D, D)
snaps2 (x, y) = snapshot const (x, y) trigger
    where trigger = sigToEvt $ changed [x, y]

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

-- | An event stram of lists of random values in the interval @(0, 1)@.
-- The first argument is the length of the each list.
randList :: Int -> Evt b -> Evt [D]
randList n = accumSE (0 :: D) $ const $ \s -> fmap (, s) $ 
    sequence $ replicate n $ fmap readSnap $ random (0::D) 1

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


-- converting to toggle signals

togGen :: D -> Tick -> Evt D
togGen n = accumE n (\_ s -> let v = (mod' (s + 1) 2) in (v, v))

-- | Converts clicks to alternating 0 and 1 (toggle event stream)
toTog :: Tick -> Evt D
toTog  = togGen 1

-- | Converts clicks to alternating 1 and 0 (toggle event stream with first value set to 1)
toTog1 :: Tick -> Evt D
toTog1 = togGen 0


mkRow :: Evt a -> Evt (a, D)
mkRow = accumE (0 :: D) (\a s -> ((a, s), s + 1) )

filterRow :: (D -> BoolD) -> Evt a -> Evt a
filterRow p = fmap fst . filterE (p . snd) . mkRow

-- | Takes the ns events from the event stream and ignores the rest of the stream.
takeE :: Int -> Evt a -> Evt a
takeE n = filterRow ( <* int n)

-- | Drops the ns events from the event stream and leaves the rest of the stream.
dropE :: Int -> Evt a -> Evt a
dropE n = filterRow ( >=* int n)

-- | Takes events while the predicate is true.
takeWhileE :: (a -> BoolD) -> Evt a -> Evt a
takeWhileE p = fmap fst . filterE snd . accumE (1 :: D) (\a s -> let s1 = s ==* 1 &&* p a in ((a, s1), ifB s1 1 0)) 

-- | Drops events while the predicate is true.
dropWhileE :: (a -> BoolD) -> Evt a -> Evt a
dropWhileE p = fmap fst . filterE (notB . snd) . accumE (1 :: D) (\a s -> let s1 = s ==* 1 &&* p a in ((a, s1), ifB s1 1 0)) 

